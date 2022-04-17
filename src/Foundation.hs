{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import PathPiece()

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types
import Yesod.Auth.Message
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Network.Wai as Wai

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    } deriving (Typeable)

mkYesodData "App" $(parseRoutesFile "config/routes")

deriving instance Typeable Route 
deriving instance Generic (Route App)

-- YesodPersist

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action (appConnPool master)

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

session_timeout_minutes :: Int
session_timeout_minutes = 10080 -- (7 days)

-- Yesod

instance Yesod App where
    approot = ApprootRequest \app req ->
        case appRoot (appSettings app) of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend App {appSettings} = do
      backend <-
        defaultClientSessionBackend
          session_timeout_minutes
          "config/client_session_key.aes"
      maybeSSLOnly $ pure (Just backend)
      where
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
            -- For the time being, `AddR` is the only route that accepts an
            -- authentication token.
            Just AddR -> isJust <$> lookupHeader "Authorization"
            _ -> pure False
          (if dontCheckCsrf then id else defaultCsrfMiddleware) handler

        customMiddleware handler = do
          addHeader "X-Frame-Options" "DENY"
          yesod <- getYesod
          (if appSSLOnly (appSettings yesod)
             then sslOnlyMiddleware session_timeout_minutes
             else id) handler

    defaultLayout widget = do
        req <- getRequest
        master <- getYesod
        urlrender <- getUrlRender
        mmsg <- getMessage
        musername <- maybeAuthUsername
        muser <- (fmap.fmap) snd maybeAuthPair
        let msourceCodeUri = appSourceCodeUri (appSettings master)
        pc <- widgetToPageContent do
            setTitle "Espial"
            addAppScripts
            addStylesheet (StaticR css_tachyons_min_css)
            addStylesheet (StaticR css_main_css)
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir (appSettings master)
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLogIO app _source level =
        pure $ appShouldLogAll (appSettings app) || level == LevelWarn || level == LevelError
    makeLogger = return . appLogger

    authRoute _ = Just (AuthR LoginR)

    isAuthorized (AuthR _) _ = pure Authorized
    isAuthorized _ _ = pure Authorized

    defaultMessageWidget title body = do
      setTitle title
      toWidget [hamlet|
        <main .pv2.ph3.mh1>
          <div .w-100.mw8.center>
            <div .pa3.bg-near-white>
              <h1>#{title}
              ^{body}
      |]


isAuthenticated :: Handler AuthResult
isAuthenticated = maybeAuthId >>= \case
                    Just authId -> pure Authorized
                    _ -> pure $ AuthenticationRequired

addAppScripts :: (MonadWidget m, HandlerSite m ~ App) => m ()
addAppScripts = do
  addScript (StaticR js_app_min_js) 


-- popupLayout

popupLayout :: Widget -> Handler Html
popupLayout widget = do
    req <- getRequest
    master <- getYesod
    mmsg <- getMessage
    musername <- maybeAuthUsername
    let msourceCodeUri = appSourceCodeUri (appSettings master)
    pc <- widgetToPageContent do
      addAppScripts
      addStylesheet (StaticR css_tachyons_min_css)
      addStylesheet (StaticR css_popup_css)
      $(widgetFile "popup-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


-- YesodAuth

instance YesodAuth App where
  type AuthId App = UserId
  authPlugins _ = [dbAuthPlugin]
  authenticate = authenticateCreds
  loginDest = const HomeR
  logoutDest = const HomeR
  onLogin = maybeAuth >>= \case
    Nothing -> cpprint ("onLogin: could not find user" :: Text)
    Just (Entity _ uname) -> setSession userNameKey (userName uname)
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

instance YesodAuthPersist App

-- session keys

maybeAuthUsername :: Handler (Maybe Text)
maybeAuthUsername = do
  lookupSession userNameKey

ultDestKey :: Text
ultDestKey = "_ULT"

userNameKey :: Text
userNameKey = "_UNAME"

-- dbAuthPlugin

dbAuthPluginName :: Text
dbAuthPluginName = "db"

dbAuthPlugin :: AuthPlugin App
dbAuthPlugin = AuthPlugin dbAuthPluginName dbDispatch dbLoginHandler
  where
    dbDispatch :: Text -> [Text] -> AuthHandler App TypedContent
    dbDispatch "POST" ["login"] = dbPostLoginR >>= sendResponse
    dbDispatch _ _ = notFound
    dbLoginHandler toParent = do
      req <- getRequest
      lookupSession ultDestKey >>= \case
        Just dest | "logout" `isInfixOf` dest -> deleteSession ultDestKey
        _ -> pure ()
      setTitle "Espial | Log In"
      $(widgetFile "login")

dbLoginR :: AuthRoute
dbLoginR = PluginR dbAuthPluginName ["login"]

dbPostLoginR ::  AuthHandler master TypedContent
dbPostLoginR = do
  mresult <- runInputPostResult (dbLoginCreds
               <$> ireq textField "username"
               <*> ireq textField "password")
  case mresult of
    FormSuccess creds -> setCredsRedirect creds
    _ -> loginErrorMessageI LoginR InvalidUsernamePass

dbLoginCreds :: Text -> Text -> Creds master
dbLoginCreds username password =
  Creds
  { credsPlugin = dbAuthPluginName
  , credsIdent = username
  , credsExtra = [("password", password)]
  }

authenticateCreds ::
     (MonadHandler m, HandlerSite m ~ App)
  => Creds App
  -> m (AuthenticationResult App)
authenticateCreds Creds {..} = do
  muser <-
    case credsPlugin of
      p | p == dbAuthPluginName -> liftHandler $ runDB $
        join <$> mapM (\pwd -> authenticatePassword credsIdent pwd) (lookup "password" credsExtra)
      _ -> pure Nothing
  case muser of
    Nothing -> pure (UserError InvalidUsernamePass)
    Just (Entity uid _) -> pure (Authenticated uid)

-- Util

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

