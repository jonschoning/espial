{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Foundation where

import Archiver.Backend (ArchiveQueue, ArchiverBackend)
import ClassyPrelude.Yesod qualified as CP (Lang)
import Control.Concurrent (ThreadId)
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Char (isSpace)
import Data.IP (fromSockAddr)
import Data.List (dropWhileEnd)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (NominalDiffTime, diffUTCTime)
import Data.Type.Equality (type (~))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Network.Wai qualified as Wai
import PathPiece ()
import System.Environment (lookupEnv)
import Text.Hamlet (hamletFile)
import Yesod.Auth.Message
import Yesod.Core.Types
import Yesod.Core.Unsafe qualified as Unsafe

-- * App

type PublicTagCloudCache = IORef (Map (Text, TagCloudMode) (UTCTime, Map Text Int))

-- | Sliding-window login-attempt counters, keyed by e.g. @"ip:1.2.3.4"@ or @"user:alice"@.
type LoginRateLimiter = IORef (Map Text (UTCTime, Int))

-- | Serializes DB write transactions; see 'appDBWriteLock'.
newtype DBWriteLock = DBWriteLock (MVar ())

newDBWriteLock :: (MonadIO m) => m DBWriteLock
newDBWriteLock = DBWriteLock <$> newMVar ()

data App = App
  { appSettings :: AppSettings,
    -- | Settings for static file serving.
    appStatic :: Static,
    -- | Frontend ESM bundle filename under static/js.
    appFrontendBundleName :: Text,
    -- | Database connection pool.
    appConnPool :: ConnectionPool,
    -- | HTTP manager, used for making HTTP requests to external services.
    appHttpManager :: Manager,
    -- | Logger
    appLogger :: Logger,
    -- | Active archiver plugin paired with its job queue
    appArchiver :: Maybe (ArchiverBackend, ArchiveQueue),
    -- | Thread running 'Archiver.Backend.runArchiveQueueWorker'; killed by
    -- 'Application.shutdownApp' so dev hot-reloads don't leak worker threads.
    appArchiveWorkerThreadId :: Maybe ThreadId,
    -- | i18n translation function
    appTranslate :: I18nLang -> I18nKey -> Text,
    -- | Route to the i18n json data
    appI18nR :: Route App,
    -- | In-memory cache for public tag cloud responses (30s TTL, 1000-entry cap).
    appPublicTagCloudCache :: PublicTagCloudCache,
    -- | Login-attempt counters used to throttle the login endpoint.
    appLoginRateLimiter :: LoginRateLimiter,
    -- | Serializes DB write transactions to avoid SQLite `SQLITE_BUSY`/snapshot conflicts under concurrent writers.
    appDBWriteLock :: DBWriteLock
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

deriving instance Generic (Route App)

-- YesodPersist

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    app <- getYesod
    runSqlPool action (appConnPool app)

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

-- | Like 'runDB', but holds 'appDBWriteLock' for the duration of the transaction.
--
-- (SQLite allows only one writer at a time; under concurrent writers, a read
-- promoted to a write mid-transaction can hit a snapshot conflict that SQLite
-- reports as `SQLITE_BUSY` without retrying (unlike ordinary lock contention))
runDBWrite :: ReaderT SqlBackend Handler a -> Handler a
runDBWrite action = do
  App {appDBWriteLock, appSettings = AppSettings {appSqliteAppWriteLock}} <- getYesod
  withDBWriteLock appSqliteAppWriteLock appDBWriteLock (runDB action)

withDBWriteLock :: (MonadUnliftIO m) => Bool -> DBWriteLock -> m a -> m a
withDBWriteLock writeLockEnabled (DBWriteLock writeLock) action =
  if writeLockEnabled
    then withMVar writeLock $ \_ -> action
    else action

session_timeout_minutes :: Int
session_timeout_minutes = 10080 -- (7 days)

-- * Yesod App

instance Yesod App where
  approot = ApprootRequest \app req ->
    case appRoot (appSettings app) of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend App {appSettings} = do
    envKey <- lookupEnv "CLIENT_SESSION_KEY"
    backend <- case envKey of
      Just key | not (null (stripString key)) -> envClientSessionBackend session_timeout_minutes "CLIENT_SESSION_KEY"
      _ ->
        defaultClientSessionBackend
          session_timeout_minutes
          "config/client_session_key.aes"
    maybeSSLOnly $ pure (Just backend)
    where
      stripString :: String -> String
      stripString = dropWhile isSpace . dropWhileEnd isSpace
      maybeSSLOnly =
        if appSSLOnly appSettings
          then sslOnlySessions
          else id

  yesodMiddleware :: HandlerFor App res -> HandlerFor App res
  yesodMiddleware = customMiddleware . defaultYesodMiddleware . customCsrfMiddleware
    where
      customCsrfMiddleware handler = do
        maybeRoute <- getCurrentRoute
        dontCheckCsrf <- case maybeRoute of
          -- `maybeAuthId` checks for the validity of the Authorization
          -- header anyway, but it is still a good idea to limit this
          -- flexibility to designated routes.
          -- For the time being, `AddR` and `AddBulkR` are the only routes that accept an
          -- authentication token.
          Just AddR -> isJust <$> lookupHeader "Authorization"
          Just AddBulkR -> isJust <$> lookupHeader "Authorization"
          _ -> pure False
        (if dontCheckCsrf then id else defaultCsrfMiddleware) handler

      customMiddleware handler = do
        addHeader "X-Frame-Options" "DENY"
        yesod <- getYesod
        ( if appSSLOnly (appSettings yesod)
            then sslOnlyMiddleware session_timeout_minutes
            else id
          )
          handler

  jsAttributes _ = [("type", "module")]

  maximumContentLength :: App -> Maybe (Route App) -> Maybe Word64
  maximumContentLength app _
    | n <= 0 = Nothing
    | otherwise = Just (fromIntegral n)
    where
      n = appMaximumContentLength (appSettings app)

  shouldLogIO app "yesod-core" LevelWarn = pure False -- remove verbose CSRF token warnings
  shouldLogIO app "startup" level =
    pure
      ( ( appEnableStartupLogging (appSettings app)
            && (appShouldLogAll (appSettings app) || level == LevelInfo || level == LevelWarn || level == LevelError)
        )
          || level == LevelWarn
          || level == LevelError
      )
  shouldLogIO app source level = pure $ appShouldLogAll (appSettings app) || level == LevelWarn || level == LevelError

  makeLogger = pure . appLogger

  authRoute _ = Just (AuthR LoginR)

  isAuthorized (AuthR _) _ = pure Authorized
  isAuthorized _ _ = pure Authorized

  errorHandler :: ErrorResponse -> HandlerFor App TypedContent
  errorHandler err = do
    app <- getYesod
    lang <- getCurrentLang LangSourceSession
    let t = \key -> appTranslate app lang (I18nKey key)
    case err of
      NotFound -> _notFoundErrorHandler (t "error.notFound")
      InternalError e -> _internalErrorHandler e (t "error.internalError")
      NotAuthenticated -> _notAuthenticatedErrorHandler (t "error.notAuthenticated")
      BadMethod _ -> _genericErrorHandler (t "error.badMethod")
      InvalidArgs _ -> _genericErrorHandler (t "error.invalidArgs")
      PermissionDenied _ -> _genericErrorHandler (t "error.permissionDenied")
    where
      _notFoundErrorHandler :: Text -> HandlerFor App TypedContent
      _notFoundErrorHandler title = do
        selectRep $ do
          provideRep $ defaultLayout $ do
            r <- waiRequest
            let path' = TE.decodeUtf8With TEE.lenientDecode $ Wai.rawPathInfo r
            defaultMessageWidget (toHtml title) [hamlet|<p>#{path'}|]
          provideRep $ pure $ object ["message" .= title]
          provideRep $ pure title
      _internalErrorHandler :: Text -> Text -> HandlerFor App TypedContent
      _internalErrorHandler e title = do
        $logErrorS "yesod-core" e
        selectRep $ do
          provideRep $ defaultLayout $ do
            defaultMessageWidget (toHtml title) [hamlet|<pre>#{e}|]
          provideRep $ pure $ object ["message" .= title, "error" .= e]
          provideRep $ pure $ title <> ": " <> e
      _notAuthenticatedErrorHandler :: Text -> HandlerFor App TypedContent
      _notAuthenticatedErrorHandler title = selectRep $ do
        provideRep $
          defaultLayout $
            defaultMessageWidget (toHtml title) [hamlet|<p style="display:none;"><p>#{title} |]
        provideRep $ do
          addHeader "WWW-Authenticate" "RedirectJSON realm=\"application\", param=\"authentication_url\""
          site <- getYesod
          rend <- getUrlRender
          let apair u = ["authentication_url" .= rend u]
              content = maybe [] apair (authRoute site)
          pure $ object $ ("message" .= title) : content
        provideRep $ pure title
      _genericErrorHandler :: Text -> HandlerFor App TypedContent
      _genericErrorHandler title = do
        selectRep $ do
          provideRep $ defaultLayout $ do
            defaultMessageWidget (toHtml title) [hamlet|<p>#{title}|]
          provideRep $ pure $ object ["message" .= title]
          provideRep $ pure title

  defaultMessageWidget :: Html -> HtmlUrl (Route App) -> WidgetFor App ()
  defaultMessageWidget title body = do
    setTitle title
    toWidget
      [hamlet|
        <main .pv2.ph3.mh1>
          <div .w-100.mw8.center>
            <div .pa3.thm-bg-secondary>
              <h1>#{title}
              ^{body}
      |]

  defaultLayout :: WidgetFor App () -> HandlerFor App Html
  defaultLayout widget = do
    req <- getRequest
    app <- getYesod
    urlrender <- getUrlRender
    mmsg <- getMessage
    musername <- maybeAuthUsername
    muser <- (fmap . fmap) snd maybeAuthPair
    lang <- getCurrentLang (LangSourceUserOrSession muser)
    let msourceCodeUri = appSourceCodeUri (appSettings app)
        frontendBundleName = appFrontendBundleName app
        t = \key -> appTranslate app lang (I18nKey key)
        i18nR = appI18nR app
    pc <- widgetToPageContent do
      setTitle "Espial"
      addStylesheet (StaticR css_tachyons_min_css)
      addStylesheet (StaticR css_common_css)
      addStylesheet (StaticR css_main_css)
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

popupLayout :: Widget -> Handler Html
popupLayout widget = do
  req <- getRequest
  app <- getYesod
  mmsg <- getMessage
  muser <- fmap entityVal <$> maybeAuth
  lang <- getCurrentLang (LangSourceUserOrSession muser)
  let musername = fmap userName muser
      msourceCodeUri = appSourceCodeUri (appSettings app)
      frontendBundleName = appFrontendBundleName app
      t = \key -> appTranslate app lang (I18nKey key)
      i18nR = appI18nR app
  pc <- widgetToPageContent do
    setTitle "Espial"
    addStylesheet (StaticR css_tachyons_min_css)
    addStylesheet (StaticR css_common_css)
    addStylesheet (StaticR css_popup_css)
    $(widgetFile "popup-layout")
  withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- | Where 'getCurrentLang' may read a language from, besides the app default.
data LangSource
  = -- | anonymous/pre-auth pages: session cookie only
    LangSourceSession
  | -- | a (possibly absent) user's saved language, ignoring the session cookie
    LangSourceUser (Maybe User)
  | -- | a (possibly absent) user's saved language, falling back to the session cookie
    LangSourceUserOrSession (Maybe User)

getCurrentLang :: (MonadHandler m, HandlerSite m ~ App) => LangSource -> m I18nLang
getCurrentLang source = do
  app <- getYesod
  let langDefault = appLanguageDefault (appSettings app)
      fromSession session = toI18nLang . decodeUtf8 =<< lookup "_LANG" session
  case source of
    LangSourceSession -> fromMaybe langDefault . fromSession <$> getSession
    LangSourceUser muser -> pure (fromMaybe langDefault (muser >>= userLanguage))
    LangSourceUserOrSession muser -> do
      session <- getSession
      pure (fromMaybe langDefault ((muser >>= userLanguage) <|> fromSession session))

-- * YesodAuth App

instance YesodAuth App where
  type AuthId App = UserId
  authPlugins _ = [dbAuthPlugin]
    where
      dbAuthPlugin :: AuthPlugin App
      dbAuthPlugin = AuthPlugin dbAuthPluginName dbDispatch dbLoginHandler
        where
          dbDispatch :: Text -> [Text] -> AuthHandler App TypedContent
          dbDispatch "POST" ["login"] = dbPostLoginR >>= sendResponse
          dbDispatch _ _ = notFound
          dbLoginHandler toParent = do
            req <- getRequest
            currentApproot <- ($ HomeR) <$> getUrlRender
            loginR <- ($ AuthR LoginR) <$> getUrlRender
            -- Discard a stale/self-referential ultdest (old approot, or the login page
            -- itself via `redirectToReferer`) rather than bounce there post-login.
            lookupSession ultDestKey >>= \case
              Just dest
                | "logout" `isInfixOf` dest -> deleteSession ultDestKey
                | loginR `isInfixOf` dest -> deleteSession ultDestKey
                | not (currentApproot `isPrefixOf` dest) -> deleteSession ultDestKey
              _ -> pure ()
            app <- getYesod
            lang <- getCurrentLang LangSourceSession
            let t = \key -> appTranslate app lang (I18nKey key)
            setTitle (toHtml ("Espial | " <> t "login.pageTitle"))
            $(widgetFile "login")

          dbPostLoginR :: (master ~ App) => AuthHandler master TypedContent
          dbPostLoginR = do
            app <- getYesod
            lang <- getCurrentLang LangSourceSession
            let t = \key -> appTranslate app lang (I18nKey key)
            mresult <-
              runInputPostResult
                ( dbLoginCreds
                    <$> ireq textField "username"
                    <*> ireq textField "password"
                )
            case mresult of
              FormSuccess creds -> do
                throttled <- isLoginThrottled (credsIdent creds)
                if throttled
                  then rejectLogin (t "auth.tooManyAttempts")
                  else setCredsRedirect creds
              _ -> rejectLogin (t "auth.invalidUsernamePass")
            where
              rejectLogin :: (master ~ App) => Text -> AuthHandler master TypedContent
              rejectLogin msg = loginErrorMessage (AuthR LoginR) msg

              -- Checked (and recorded) before any password hashing happens, so a flood of
              -- login attempts against one IP or one username gets rejected without paying
              -- the hashing cost.
              isLoginThrottled :: (master ~ App) => Text -> AuthHandler master Bool
              isLoginThrottled username = do
                app <- getYesod
                req <- waiRequest
                let AppSettings {..} = appSettings app
                    window = fromIntegral appLoginRateLimitWindowSeconds
                    ip = clientIpText appIpFromHeader req
                byIp <- liftIO $ checkLoginRateLimit (appLoginRateLimiter app) appLoginRateLimitMaxAttempts window ("ip:" <> ip)
                byUser <- liftIO $ checkLoginRateLimit (appLoginRateLimiter app) appLoginRateLimitMaxAttempts window ("user:" <> username)
                pure (byIp || byUser)

              dbLoginCreds :: Text -> Text -> Creds master
              dbLoginCreds username password =
                Creds
                  { credsPlugin = dbAuthPluginName,
                    credsIdent = username,
                    credsExtra = [("password", password)]
                  }

          ultDestKey :: Text
          ultDestKey = "_ULT"

  authenticate = authenticateCreds
    where
      authenticateCreds ::
        (MonadHandler m, HandlerSite m ~ App) =>
        Creds App ->
        m (AuthenticationResult App)
      authenticateCreds Creds {..} = do
        muser <- case lookup "password" credsExtra of
          Just pwd | credsPlugin == dbAuthPluginName -> do
            rehashAlgo <- appPasswordHashConfig . appSettings <$> getYesod
            liftHandler $ runDBWrite $ authenticatePassword rehashAlgo credsIdent pwd
          _ -> pure Nothing
        case muser of
          Nothing -> pure (UserError InvalidUsernamePass)
          Just (Entity uid _) -> pure (Authenticated uid)
  loginDest = const HomeR
  logoutDest = const HomeR
  onLogin =
    maybeAuth >>= \case
      Nothing -> $(logWarn) "onLogin: could not find user"
      Just (Entity user uname) -> do
        lang <- getCurrentLang (LangSourceUser (Just uname))
        setSession userNameKey (userName uname)
        setLanguage (fromI18nLang lang)
  onLogout =
    deleteSession userNameKey
  redirectToReferer = const True
  maybeAuthId = do
    req <- waiRequest
    let mAuthHeader = lookup "Authorization" (Wai.requestHeaders req)
        extractKey = stripPrefix "ApiKey " . TE.decodeUtf8
    case mAuthHeader of
      Just authHeader ->
        case extractKey authHeader of
          Just apiKey -> do
            user <- liftHandler $ runDB $ getApiKeyUser (ApiKey apiKey)
            let userId = entityKey <$> user
            pure userId
          -- Since we disable CSRF middleware in the presence of Authorization
          -- header, we need to explicitly check for the validity of the header
          -- content. Otherwise, a dummy Authorization header with garbage input
          -- could be provided to circumvent CSRF token requirement, making the app
          -- vulnerable to CSRF attacks.
          Nothing -> pure Nothing
      _ -> defaultMaybeAuthId
  renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
  renderAuthMessage app langs msg = do
    let settings = appSettings app
    pickLang (langs ++ [CI.foldedCase $ fromI18nLang (appLanguageDefault settings)])
    where
      pickLang [] = englishMessage msg
      pickLang (l : ls) = case l of
        "en" -> englishMessage msg
        "cs" -> czechMessage msg
        "da" -> danishMessage msg
        "de" -> germanMessage msg
        "es" -> spanishMessage msg
        "fi" -> finnishMessage msg
        "fr" -> frenchMessage msg
        "hr" -> croatianMessage msg
        "ja" -> japaneseMessage msg
        "ko" -> koreanMessage msg
        "nb" -> norwegianBokmålMessage msg
        "nl" -> dutchMessage msg
        "pt" -> portugueseMessage msg
        "pl" -> pickLang ("cs" : ls) -- TODO
        "ro" -> romanianMessage msg
        "ru" -> russianMessage msg
        "sv" -> swedishMessage msg
        "zh" -> chineseMessage msg
        _ -> pickLang ls

-- | Client IP for rate-limiting purposes: the first @x-real-ip@ or @x-forwarded-for@
-- header value when trusted (i.e. behind a reverse proxy), else the raw peer address.
clientIpText :: Bool -> Wai.Request -> Text
clientIpText trustHeaders req =
  fromMaybe peerIp (if trustHeaders then headerIp else Nothing)
  where
    peerIp = maybe "unknown" (tshow . fst) (fromSockAddr (Wai.remoteHost req))
    headerIp =
      TE.decodeUtf8With TEE.lenientDecode . BS8.takeWhile (/= ',')
        <$> (lookup "x-real-ip" hdrs <|> lookup "x-forwarded-for" hdrs)
    hdrs = Wai.requestHeaders req

-- | Records this attempt for @key@ and reports whether it has exceeded @maxAttempts@
-- within the trailing @window@. Prunes expired entries and caps map size the same way
-- as the public tag cloud cache above.
checkLoginRateLimit :: LoginRateLimiter -> Int -> NominalDiffTime -> Text -> IO Bool
checkLoginRateLimit ref maxAttempts window key = do
  !now <- getCurrentTime
  atomicModifyIORef' ref $ \m ->
    let pruned = Map.filter (\(t, _) -> diffUTCTime now t < window) m
        evicted = if Map.size pruned >= 5000 then Map.deleteMin pruned else pruned
     in case Map.lookup key evicted of
          Just (windowStart, attempts) ->
            let !attempts' = attempts + 1
                !throttled = attempts' > maxAttempts
             in (Map.insert key (windowStart, attempts') evicted, throttled)
          Nothing -> (Map.insert key (now, 1 :: Int) evicted, False)

instance YesodAuthPersist App

-- instance RenderMessage App AuthMessage where
--   renderMessage :: App -> [CP.Lang] -> AuthMessage -> Text
--   renderMessage _ _ msg = defaultMessage msg

instance RenderMessage App FormMessage where
  renderMessage :: App -> [CP.Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

-- session keys

maybeAuthUsername :: Handler (Maybe Text)
maybeAuthUsername = do
  lookupSession userNameKey

userNameKey :: Text
userNameKey = "_UNAME"

-- dbAuthPlugin

dbAuthPluginName :: Text
dbAuthPluginName = "db"

dbLoginR :: AuthRoute
dbLoginR = PluginR dbAuthPluginName ["login"]

-- Util

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger