{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import PathPiece()

-- import Yesod.Auth.Dummy

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types
import Yesod.Auth.Message
import qualified Network.Wai as NW
import qualified Control.Monad.Metrics as MM
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Yesod.Core.Unsafe as Unsafe

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appMetrics     :: !MM.Metrics
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

-- Yesod

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot (appSettings app) of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        10080 -- min (7 days)
        "config/client_session_key.aes"

    yesodMiddleware = metricsMiddleware . defaultYesodMiddleware . defaultCsrfMiddleware

    defaultLayout widget = do
        req <- getRequest
        master <- getYesod
        urlrender <- getUrlRender
        mmsg <- getMessage
        musername <- maybeAuthUsername
        muser <- (fmap.fmap) snd maybeAuthPair
        mcurrentRoute <- getCurrentRoute
        void $ mapM (incrementRouteEKG req) mcurrentRoute
        pc <- widgetToPageContent $ do
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
    pc <- widgetToPageContent $ do
      addAppScripts
      addStylesheet (StaticR css_tachyons_min_css)
      addStylesheet (StaticR css_popup_css)
      $(widgetFile "popup-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


metricsMiddleware :: Handler a -> Handler a
metricsMiddleware handler = do
  req <- getRequest
  mcurrentRoute <- getCurrentRoute
  void $ mapM (incrementRouteEKG req) mcurrentRoute
  handler


incrementRouteEKG :: YesodRequest -> Route App -> Handler ()
incrementRouteEKG req = MM.increment . (\r -> "route." <> r <> "." <> method) . pack . constrName
  where method = decodeUtf8 $ NW.requestMethod $ reqWaiRequest req

-- YesodAuth

instance YesodAuth App where
  type AuthId App = UserId
  -- authHttpManager = getHttpManager
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

instance YesodAuthPersist App

instance MM.MonadMetrics Handler where
  getMetrics = pure . appMetrics =<< getYesod 

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
        join <$> mapM (authenticatePassword credsIdent) (lookup "password" credsExtra)
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

