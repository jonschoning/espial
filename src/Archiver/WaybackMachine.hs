{-# LANGUAGE MultiWayIf #-}

module Archiver.WaybackMachine
  ( waybackMachineBackend,
  )
where

import Archiver.Backend
import ClassyPrelude
import Control.Monad.Logger (LoggingT, logDebug, runLoggingT)
import Data.ByteString qualified as BS
import Database.Persist.Sql (Key)
import Model (Bookmark, Url (..), User, UserAgent (UserAgent), updateBookmarkArchiveUrl)
import Network.HTTP.Client qualified as NH
import Network.HTTP.Types.Status qualified as NH
import Web.FormUrlEncoded qualified as WH
import Yesod.Default.Main (LogFunc)

-- | Wayback Machine backend.
waybackMachineBackend :: Text -> Text -> ArchiverDB -> NH.Manager -> UserAgent -> LogFunc -> ArchiverBackend
waybackMachineBackend accessKey secretKey archiverDB manager userAgent logFunc =
  ArchiverBackend
    { runArchiver = \uid bid url -> flip runLoggingT logFunc $ _waybackMachineRun accessKey secretKey archiverDB manager userAgent uid bid url,
      isUrlDenylisted = \(Url url) -> any (`isInfixOf` url) _waybackMachineDenylist
    }

_waybackMachineDenylist :: [Text]
_waybackMachineDenylist =
  [ "archive.org",
    "web.archive.org"
  ]

_waybackMachineRun :: Text -> Text -> ArchiverDB -> NH.Manager -> UserAgent -> Key User -> Key Bookmark -> Url -> LoggingT IO ()
_waybackMachineRun accessKey secretKey ArchiverDB {archiverRunDBWrite} manager userAgent userId bookmarkId url = do
  $(logDebug) $ "Archiving URL with Wayback Machine: " <> unUrl url
  let req = _buildWaybackMachineSubmitRequest accessKey secretKey userAgent url
  $(logDebug) $ "Wayback Machine request: " <> tshow req
  res <- liftIO $ NH.httpLbs req manager
  let status = NH.responseStatus res
      mArchiveUrl =
        if
          | status == NH.status200 -> Just (_extractArchiveUrl url)
          | status == NH.status302 || status == NH.status307 -> Just (_extractArchiveUrl url)
          | otherwise -> Nothing
  $(logDebug) $ "Archive response status: " <> tshow status <> ", URL result: " <> tshow mArchiveUrl
  $(logDebug) $ "Archive response body: " <> tshow (NH.responseBody res)
  forM_ mArchiveUrl $ \archiveUrl ->
    liftIO $ archiverRunDBWrite (updateBookmarkArchiveUrl userId bookmarkId (Just archiveUrl))

_buildWaybackMachineSubmitRequest :: Text -> Text -> UserAgent -> Url -> NH.Request
_buildWaybackMachineSubmitRequest accessKey secretKey (UserAgent ua) (Url href) =
  let body =
        WH.urlEncodeAsForm
          ( [ ("url", unpack href),
              ("capture_all", "on"),
              ("capture_screenshot", "on")
            ] ::
              [(String, String)]
          )
      req = NH.parseRequest_ "https://web.archive.org/save"
   in req
        { NH.method = "POST",
          NH.requestHeaders =
            [ ("Authorization", BS.concat ["LOW ", encodeUtf8 accessKey, ":", encodeUtf8 secretKey]),
              ("Accept", "application/json"),
              ("Content-Type", "application/x-www-form-urlencoded"),
              ("User-Agent", encodeUtf8 ua)
            ],
          NH.requestBody = NH.RequestBodyLBS body,
          NH.redirectCount = 0
        }

_extractArchiveUrl :: Url -> Text
_extractArchiveUrl (Url inputUrl) = "https://web.archive.org/web/*/" <> inputUrl
