-- | Archive backend that renders a page in a remote chromium (reached over the Chrome
-- DevTools Protocol) and hands the JS-rendered DOM to @monolith@ to inline into a single
-- self-contained HTML file. No node/single-file-cli involved; the only external process is
-- the @monolith@ binary (required on the host/image running this backend), and the only new
-- library dependency is a plain CDP-over-websocket client written against this narrow use case.
module Archiver.Chromium
  ( chromiumBackend,
  )
where

import Archiver.Backend
import Archiver.LocalArchive
import ClassyPrelude
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (LoggingT, logDebug, logWarn, runLoggingT)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Database.Persist.Sql (Key)
import Model (Bookmark, Url (..), User, updateBookmarkArchiveUrl)
import Network.HTTP.Client
  ( Manager,
    defaultManagerSettings,
    httpLbs,
    method,
    newManager,
    parseRequest,
    responseBody,
  )
import Network.HTTP.Types.URI qualified as NUri
import Network.Socket qualified as NS
import Network.WebSockets qualified as WS
import Settings (AppSettings (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, removeFile, renamePath)
import System.Exit (ExitCode (..))
import Yesod.Default.Main (LogFunc)

data ChromiumEndpoint = ChromiumEndpoint
  { chromiumEndpointHost :: String,
    chromiumEndpointPort :: Int
  }

data ChromiumContext = ChromiumContext
  { chromiumEndpoint :: ChromiumEndpoint,
    chromiumDir :: FilePath,
    chromiumWaitMicros :: Int,
    chromiumTimeoutMicros :: Int,
    chromiumMonolithTimeoutMicros :: Int,
    chromiumMonolithExe :: FilePath,
    chromiumMonolithArgs :: [String],
    chromiumHttpManager :: Manager,
    chromiumDB :: ArchiverDB
  }

chromiumBackend :: AppSettings -> ArchiverDB -> LogFunc -> IO (Maybe ArchiverBackend)
chromiumBackend AppSettings {..} archiverDB logFunc = flip runLoggingT logFunc $ do
  let monolithExe = unpack (T.strip appMonolithPath)
  case parseCdpUrl appChromiumCdpUrl of
    Nothing -> do
      $(logWarn) "Archive backend `chromium` selected but chromium-cdp-url is missing or invalid; archiving disabled"
      pure Nothing
    Just endpoint -> do
      dirOk <- liftIO $ tryAny (createDirectoryIfMissing True appChromiumDir)
      exeOk <- liftIO $ tryAny (probeExe monolithExe ["--version"])
      case (dirOk, exeOk) of
        (Left e, _) -> do
          $(logWarn) $ "Archive backend `chromium` selected but output dir " <> pack appChromiumDir <> " is not writable (" <> tshow e <> "); archiving disabled"
          pure Nothing
        (_, Left e) -> do
          $(logWarn) $ "Archive backend `chromium` selected but " <> pack monolithExe <> " could not be run (" <> tshow e <> "); archiving disabled"
          pure Nothing
        (_, Right Nothing) -> do
          $(logWarn) $ "Archive backend `chromium` selected but " <> pack monolithExe <> " --version timed out; archiving disabled"
          pure Nothing
        (_, Right (Just code)) | code /= ExitSuccess -> do
          $(logWarn) $ "Archive backend `chromium` selected but " <> pack monolithExe <> " --version exited with " <> tshow code <> "; archiving disabled"
          pure Nothing
        _ -> do
          manager <- liftIO $ newManager defaultManagerSettings
          let ctx =
                ChromiumContext
                  { chromiumEndpoint = endpoint,
                    chromiumDir = appChromiumDir,
                    chromiumWaitMicros = appChromiumWaitMs * 1000,
                    chromiumTimeoutMicros = appChromiumTimeoutSec * 1000000,
                    chromiumMonolithTimeoutMicros = appMonolithTimeoutSec * 1000000,
                    chromiumMonolithExe = monolithExe,
                    chromiumMonolithArgs = map unpack (words appChromiumMonolithArgs),
                    chromiumHttpManager = manager,
                    chromiumDB = archiverDB
                  }
          pure $
            Just
              ArchiverBackend
                { runArchiver = \uid bid url -> flip runLoggingT logFunc $ _chromiumRun ctx uid bid url,
                  isUrlDenylisted = \(Url url) ->
                    let scheme = toLower (T.takeWhile (/= ':') url)
                     in not (scheme == "http" || scheme == "https")
                }

-- | Parses e.g. @http://browser:9222@ into a host/port pair. Scheme is ignored (CDP's HTTP
-- endpoint is never TLS); port defaults to chromium's usual debugging port when omitted.
parseCdpUrl :: Text -> Maybe ChromiumEndpoint
parseCdpUrl raw =
  let stripped = T.strip raw
      noScheme = fromMaybe stripped (T.stripPrefix "http://" stripped <|> T.stripPrefix "https://" stripped)
      authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') noScheme
      (hostPart, colonPort) = T.breakOnEnd ":" authority
   in if T.null authority
        then Nothing
        else case (T.stripSuffix ":" hostPart, readMay (T.unpack colonPort) :: Maybe Int) of
          (Just h, Just p) | not (T.null h) -> Just (ChromiumEndpoint (T.unpack h) p)
          _ -> Just (ChromiumEndpoint (T.unpack authority) 9222)

-- | Chrome's devtools HTTP/WS endpoints reject a non-numeric Host header (anti DNS-rebinding
-- protection), so a compose service name like @browser@ has to be resolved to an IP first.
resolveHostIp :: String -> IO (Maybe String)
resolveHostIp host = do
  addrs <- tryAny $ NS.getAddrInfo (Just NS.defaultHints {NS.addrSocketType = NS.Stream}) (Just host) Nothing
  case addrs of
    Left _ -> pure Nothing
    Right [] -> pure Nothing
    Right (addr : _) -> do
      nameInfo <- tryAny $ NS.getNameInfo [NS.NI_NUMERICHOST] True False (NS.addrAddress addr)
      pure $ case nameInfo of
        Right (Just ip, _) -> Just ip
        _ -> Nothing

_chromiumRun :: ChromiumContext -> Key User -> Key Bookmark -> Url -> LoggingT IO ()
_chromiumRun ChromiumContext {..} userId bookmarkId (Url url) = do
  let out = localArchiveFile chromiumDir userId bookmarkId
      tmp = out <> ".tmp"
      ChromiumEndpoint {..} = chromiumEndpoint
  $(logDebug) $ "Archiving URL with chromium: " <> url
  liftIO $ createDirectoryIfMissing True (localArchiveDir chromiumDir userId bookmarkId)
  ipResult <- liftIO $ resolveHostIp chromiumEndpointHost
  case ipResult of
    Nothing -> $(logWarn) $ "chromium: could not resolve " <> pack chromiumEndpointHost <> "; archiving skipped for " <> url
    Just ip -> do
      captureResult <-
        liftIO $
          tryAny $
            timeout chromiumTimeoutMicros $
              _captureWithChromium chromiumHttpManager ip chromiumEndpointPort chromiumWaitMicros url
      case captureResult of
        Left e -> $(logWarn) $ "chromium capture failed: " <> tshow e <> ": " <> url
        Right Nothing -> $(logWarn) $ "chromium capture timed out after " <> tshow (chromiumTimeoutMicros `div` 1000000) <> "s: " <> url
        Right (Just (Left err)) -> $(logWarn) $ "chromium capture failed: " <> err <> ": " <> url
        Right (Just (Right (finalUrl, html))) -> do
          let monolithArgs = chromiumMonolithArgs <> ["-b", unpack finalUrl, "-o", tmp, "-"]
          result <-
            liftIO $
              tryAny (runProcessWithStdin chromiumMonolithExe monolithArgs (fromStrict (encodeUtf8 html)) chromiumMonolithTimeoutMicros)
          case result of
            Left e -> discardTmp tmp $ "monolith invocation failed: " <> tshow e
            Right Nothing -> discardTmp tmp $ "monolith timed out after " <> tshow (chromiumMonolithTimeoutMicros `div` 1000000) <> "s: " <> url
            Right (Just code) | code /= ExitSuccess -> discardTmp tmp $ "monolith exited with " <> tshow code <> ": " <> url
            Right _ -> do
              size <- liftIO $ tryAny (getFileSize tmp)
              case size of
                Right n | n > 0 -> do
                  liftIO $ renamePath tmp out
                  let href = localArchiveHref bookmarkId
                  $(logDebug) $ "storing archive link: " <> href
                  liftIO $ archiverRunDBWrite chromiumDB (updateBookmarkArchiveUrl userId bookmarkId (Just href))
                _ -> discardTmp tmp $ "monolith produced no output for " <> url
  where
    discardTmp tmp warning = do
      $(logWarn) warning
      liftIO $ void $ tryAny $ whenM (doesFileExist tmp) (removeFile tmp)

-- | Creates a page at @url@ in the remote chromium, waits for it to finish loading, then
-- returns its final URL (after any redirects) and its post-JS-rendered @outerHTML@.
_captureWithChromium :: Manager -> String -> Int -> Int -> Text -> IO (Either Text (Text, Text))
_captureWithChromium manager ip port waitMicros url = do
  let baseUrl = "http://" <> ip <> ":" <> show port
  newTargetResult <- tryAny $ do
    req <- parseRequest (baseUrl <> "/json/new?" <> unpack (decodeUtf8 (NUri.urlEncode True (encodeUtf8 url))))
    httpLbs req {method = "PUT"} manager
  case newTargetResult of
    Left e -> pure (Left ("could not create chromium target: " <> tshow e))
    Right resp ->
      case A.decode (responseBody resp) of
        Just (A.Object o) | Just (A.String targetId) <- KM.lookup "id" o ->
          WS.runClient ip port ("/devtools/page/" <> unpack targetId) (_chromiumSession waitMicros)
            `finally` _closeChromiumTarget manager baseUrl targetId
        _ -> pure (Left "chromium /json/new returned an unexpected response")

-- | Closes the tab after every job (success, failure, or timeout via 'finally'). Without
-- this a page that keeps firing background requests (infinite scroll, polling, streaming)
-- would keep running in the sidecar indefinitely, and tabs would accumulate across jobs.
-- Best-effort: a failed close shouldn't mask the real capture result.
_closeChromiumTarget :: Manager -> String -> Text -> IO ()
_closeChromiumTarget manager baseUrl targetId = void $ tryAny $ do
  req <- parseRequest (baseUrl <> "/json/close/" <> unpack targetId)
  httpLbs req manager

_chromiumSession :: Int -> WS.Connection -> IO (Either Text (Text, Text))
_chromiumSession waitMicros conn = do
  nextId <- newIORef (1 :: Int)
  readyResult <- _waitForReadyState conn nextId
  case readyResult of
    Left err -> pure (Left err)
    Right () -> do
      threadDelay waitMicros
      _scrollThroughPage conn nextId
      threadDelay waitMicros
      _stripResponsiveImages conn nextId
      hrefResult <- _cdpEvaluateString conn nextId "window.location.href"
      case hrefResult of
        Left err -> pure (Left err)
        Right finalUrl -> do
          htmlResult <- _cdpEvaluateString conn nextId "document.documentElement.outerHTML"
          pure $ (finalUrl,) <$> htmlResult

-- | Scrolls to the bottom of the page in viewport-sized steps, then back to the top. Many
-- sites only mount images/sections as they scroll into view (IntersectionObserver-driven lazy
-- loading), so a single static snapshot at scrollY=0 misses everything below the fold; a short
-- pause between steps gives each newly-visible section's lazy content a chance to start loading.
_scrollThroughPage :: WS.Connection -> IORef Int -> IO ()
_scrollThroughPage conn nextId = go (20 :: Int) Nothing
  where
    go 0 _ = scrollToTop
    go n prevY = do
      result <- _cdpEvaluateString conn nextId "window.scrollBy(0, window.innerHeight); window.scrollY.toString()"
      threadDelay 250000
      case result of
        Right y | Just y /= prevY -> go (n - 1) (Just y)
        _ -> scrollToTop
    scrollToTop = void $ _cdpEvaluateString conn nextId "window.scrollTo(0, 0); ''"

-- | Strips @srcset@ from @img@/@source@ elements (resolving each @img@ to the single
-- resolution the browser actually picked via @currentSrc@ first) and drops @<link
-- rel=preload as=image imagesrcset>@ hints. Without this, monolith embeds every candidate
-- in a responsive image's srcset as its own base64 data URI rather than just the one that
-- was rendered, which on an image-heavy page multiplies output size many times over.
_stripResponsiveImages :: WS.Connection -> IORef Int -> IO ()
_stripResponsiveImages conn nextId =
  void $
    _cdpEvaluateString
      conn
      nextId
      ( "(function(){"
          <> "document.querySelectorAll('img[srcset]').forEach(function(img){"
          <> "if (img.currentSrc) { img.src = img.currentSrc; }"
          <> "img.removeAttribute('srcset');"
          <> "});"
          <> "document.querySelectorAll('source[srcset]').forEach(function(s){ s.removeAttribute('srcset'); });"
          <> "document.querySelectorAll('link[rel=preload][as=image]').forEach(function(l){ l.remove(); });"
          <> "return '';"
          <> "})()"
      )

-- | Polls @document.readyState@ until it reports @complete@; CDP has no built-in
-- "wait for load" call outside of subscribing to page-lifecycle events, and polling avoids
-- the extra bookkeeping of an event-driven receive loop for what is otherwise a
-- request/response-only connection. Also requires @location.href@ to have moved off
-- @about:blank@ first: @/json/new@'s navigation is asynchronous, so an early poll can
-- otherwise catch the target's initial blank document mid-"complete" before the real
-- navigation has even started, snapshotting nothing.
_waitForReadyState :: WS.Connection -> IORef Int -> IO (Either Text ())
_waitForReadyState conn nextId = go (100 :: Int)
  where
    go 0 = pure (Left "timed out waiting for page to finish loading")
    go n = do
      result <- _cdpEvaluateString conn nextId "location.href === 'about:blank' ? 'loading' : document.readyState"
      case result of
        Right "complete" -> pure (Right ())
        Right _ -> threadDelay 200000 >> go (n - 1)
        Left err -> pure (Left err)

_cdpEvaluateString :: WS.Connection -> IORef Int -> Text -> IO (Either Text Text)
_cdpEvaluateString conn nextId expression = do
  result <-
    _cdpCall
      conn
      nextId
      "Runtime.evaluate"
      (A.object ["expression" .= expression, "returnByValue" .= True])
  pure $ result >>= \value -> case value of
    A.Object o
      | Just (A.Object exc) <- KM.lookup "exceptionDetails" o ->
          Left ("JS evaluation failed: " <> tshow exc)
      | Just (A.Object r) <- KM.lookup "result" o,
        Just (A.String s) <- KM.lookup "value" r ->
          Right s
    _ -> Left ("unexpected Runtime.evaluate response: " <> tshow value)

_cdpCall :: WS.Connection -> IORef Int -> Text -> A.Value -> IO (Either Text A.Value)
_cdpCall conn nextId cdpMethod params = do
  reqId <- atomicModifyIORef' nextId (\n -> (n + 1, n))
  WS.sendTextData conn (A.encode (A.object ["id" .= reqId, "method" .= cdpMethod, "params" .= params]))
  _cdpAwait conn reqId

-- | Reads frames until the response matching @reqId@ arrives, discarding anything else
-- (unsolicited events are not expected here since no CDP domain's events are enabled, but a
-- stale response would otherwise wedge the loop).
_cdpAwait :: WS.Connection -> Int -> IO (Either Text A.Value)
_cdpAwait conn reqId = do
  raw <- WS.receiveData conn
  case A.decode raw of
    Just (A.Object o)
      | Just (A.Number n) <- KM.lookup "id" o,
        round n == reqId ->
          pure $ case KM.lookup "error" o of
            Just err -> Left (tshow err)
            Nothing -> Right (fromMaybe A.Null (KM.lookup "result" o))
    _ -> _cdpAwait conn reqId
