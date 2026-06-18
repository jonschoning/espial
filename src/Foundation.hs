{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Foundation where

import Archiver.Backend (ArchiverBackend)
import ClassyPrelude.Yesod qualified as CP (Lang)
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Type.Equality (type (~))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Network.Wai qualified as Wai
import PathPiece ()
import Text.Hamlet (hamletFile)
import Yesod.Auth.Message
import Yesod.Core.Types
import Yesod.Core.Unsafe qualified as Unsafe

-- * App

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
    -- | Active archiver plugin; 'Nothing' disables archiving.
    appArchiver :: Maybe ArchiverBackend,
    -- | i18n translation function
    appTranslate :: I18nLang -> I18nKey -> Text,
    -- | Route to the i18n json data
    appI18nR :: Route App
  }
  deriving (Typeable)

mkYesodData "App" $(parseRoutesFile "config/routes")

deriving instance Typeable Route

deriving instance Generic (Route App)

-- YesodPersist

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    app <- getYesod
    runSqlPool action (appConnPool app)

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

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
        ( if appSSLOnly (appSettings yesod)
            then sslOnlyMiddleware session_timeout_minutes
            else id
          )
          handler

  jsAttributes _ = [("type", "module")]

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

  makeLogger = return . appLogger

  authRoute _ = Just (AuthR LoginR)

  isAuthorized (AuthR _) _ = pure Authorized
  isAuthorized _ _ = pure Authorized

  errorHandler :: ErrorResponse -> HandlerFor App TypedContent
  errorHandler err = do
    app <- getYesod
    session <- getSession
    let lang = maybe (appLanguageDefault (appSettings app)) id (toI18nLang . decodeUtf8 =<< lookup "_LANG" session)
        t = \key -> appTranslate app lang (I18nKey key)
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
            defaultMessageWidget (fromString (unpack (title))) [hamlet|<p>#{path'}|]
          provideRep $ return $ object ["message" .= title]
          provideRep $ return title
      _internalErrorHandler :: Text -> Text -> HandlerFor App TypedContent
      _internalErrorHandler e title = do
        $logErrorS "yesod-core" e
        selectRep $ do
          provideRep $ defaultLayout $ do
            defaultMessageWidget (fromString (unpack (title))) [hamlet|<pre>#{e}|]
          provideRep $ return $ object ["message" .= title, "error" .= e]
          provideRep $ return $ title <> ": " <> e
      _notAuthenticatedErrorHandler :: Text -> HandlerFor App TypedContent
      _notAuthenticatedErrorHandler title = selectRep $ do
        provideRep $
          defaultLayout $
            defaultMessageWidget (fromString (unpack (title))) [hamlet|<p style="display:none;"><p>#{title} |]
        provideRep $ do
          addHeader "WWW-Authenticate" "RedirectJSON realm=\"application\", param=\"authentication_url\""
          site <- getYesod
          rend <- getUrlRender
          let apair u = ["authentication_url" .= rend u]
              content = maybe [] apair (authRoute site)
          return $ object $ ("message" .= title) : content
        provideRep $ return title
      _genericErrorHandler :: Text -> HandlerFor App TypedContent
      _genericErrorHandler title = do
        selectRep $ do
          provideRep $ defaultLayout $ do
            defaultMessageWidget (fromString (unpack (title))) [hamlet|<p>#{title}|]
          provideRep $ return $ object ["message" .= title]
          provideRep $ return $ title

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
    session <- getSession
    let msourceCodeUri = appSourceCodeUri (appSettings app)
        frontendBundleName = appFrontendBundleName app
        lang = fromMaybe (appLanguageDefault (appSettings app)) ((muser >>= userLanguage) <|> (toI18nLang . decodeUtf8 =<< lookup "_LANG" session))
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
  session <- getSession
  muser <- fmap entityVal <$> maybeAuth
  let musername = fmap userName muser
      msourceCodeUri = appSourceCodeUri (appSettings app)
      frontendBundleName = appFrontendBundleName app
      lang = fromMaybe (appLanguageDefault (appSettings app)) ((muser >>= userLanguage) <|> (toI18nLang . decodeUtf8 =<< lookup "_LANG" session))
      t = \key -> appTranslate app lang (I18nKey key)
      i18nR = appI18nR app
  pc <- widgetToPageContent do
    setTitle "Espial"
    addStylesheet (StaticR css_tachyons_min_css)
    addStylesheet (StaticR css_common_css)
    addStylesheet (StaticR css_popup_css)
    $(widgetFile "popup-layout")
  withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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
            lookupSession ultDestKey >>= \case
              Just dest | "logout" `isInfixOf` dest -> deleteSession ultDestKey
              _ -> pure ()
            app <- getYesod
            session <- getSession
            let lang = maybe (appLanguageDefault (appSettings app)) id (toI18nLang . decodeUtf8 =<< lookup "_LANG" session)
                t = \key -> appTranslate app lang (I18nKey key)
            setTitle ("Espial | " <> fromString (unpack (t "login.pageTitle")))
            $(widgetFile "login")

          dbPostLoginR :: (master ~ App) => AuthHandler master TypedContent
          dbPostLoginR = do
            mresult <-
              runInputPostResult
                ( dbLoginCreds
                    <$> ireq textField "username"
                    <*> ireq textField "password"
                )
            case mresult of
              FormSuccess creds -> setCredsRedirect creds
              _ -> do
                app <- getYesod
                session <- getSession
                let lang = maybe (appLanguageDefault (appSettings app)) id (toI18nLang . decodeUtf8 =<< lookup "_LANG" session)
                    t = \key -> appTranslate app lang (I18nKey key)
                loginErrorMessage (AuthR LoginR) (t "auth.invalidUsernamePass")
            where
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
        muser <- case (credsPlugin, lookup "password" credsExtra) of
          (plugin, Just pwd)
            | plugin == dbAuthPluginName ->
                liftHandler $ runDB $ authenticatePassword credsIdent pwd
          _ -> pure Nothing
        case muser of
          Nothing -> pure (UserError InvalidUsernamePass)
          Just (Entity uid _) -> pure (Authenticated uid)
  loginDest = const HomeR
  logoutDest = const HomeR
  onLogin =
    maybeAuth >>= \case
      Nothing -> cpprint ("onLogin: could not find user" :: Text)
      Just (Entity user uname) -> do
        app <- getYesod
        let lang = fromMaybe (appLanguageDefault (appSettings app)) (userLanguage uname)
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