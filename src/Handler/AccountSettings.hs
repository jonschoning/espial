module Handler.AccountSettings where

import ClassyPrelude.Yesod qualified as CP
import Import

getAccountSettingsR :: Handler Html
getAccountSettingsR = do
  app <- getYesod
  (_, user) <- requireAuthPair
  let lang = fromMaybe (appLanguageDefault (appSettings app)) (userLanguage user)
      frontendBundleName = appFrontendBundleName app
      archiveBackendEnabled = isJust (appArchiver app)
      t = \key -> appTranslate app lang (I18nKey key)
  let accountSettingsEl = "accountSettings" :: Text
  let accountSettings = toAccountSettingsForm archiveBackendEnabled user
  defaultLayout do
    $(widgetFile "user-settings")
    toWidgetBody
      [julius|
        app.userR = "@{UserR (UserNameP $ userName user)}";
        app.dat.accountSettings = #{ toJSON accountSettings } || []; 
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderAccountSettings } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        renderAccountSettings('##{accountSettingsEl}')(app.dat.accountSettings)();
    |]

postEditAccountSettingsR :: Handler ()
postEditAccountSettingsR = do
  app <- getYesod
  userId <- requireAuthId
  accountSettingsForm <- requireCheckJsonBody
  runDB (updateUserFromAccountSettingsForm userId accountSettingsForm)
  case _language accountSettingsForm of
    Nothing -> setLanguage (fromI18nLang (appLanguageDefault (appSettings app)))
    Just lang -> setLanguage (fromI18nLang lang)

getChangePasswordR :: Handler Html
getChangePasswordR = do
  void requireAuthId
  req <- getRequest
  app <- getYesod
  (_, user) <- requireAuthPair
  let lang = fromMaybe (appLanguageDefault (appSettings app)) (userLanguage user)
      t = \key -> appTranslate app lang (I18nKey key)
  defaultLayout
    $ $(widgetFile "change-password")

postChangePasswordR :: Handler Html
postChangePasswordR = do
  app <- getYesod
  (userId, user) <- requireAuthPair
  let lang = fromMaybe (appLanguageDefault (appSettings app)) (userLanguage user)
      t = \key -> appTranslate app lang (I18nKey key)
  runInputPostResult ((,) <$> ireq textField "oldpassword" <*> ireq textField "newpassword") >>= \case
    FormSuccess (old, new) -> do
      runDB (authenticatePassword (userName user) old) >>= \case
        Nothing -> setMessage (fromString (unpack (t "auth.incorrectOldPassword")))
        Just _ ->
          validateNewPassword t new >>= \case
            Just newValid -> do
              newHash <- liftIO (hashPassword newValid)
              void $ runDB (update userId [UserPasswordHash CP.=. newHash])
              setMessage (fromString (unpack (t "auth.passUpdated")))
            _ -> pure ()
    _ -> setMessage (fromString (unpack (t "auth.missingRequiredFields")))
  redirect ChangePasswordR
  where
    validateNewPassword :: (Text -> Text) -> Text -> Handler (Maybe Text)
    validateNewPassword t = \case
      new | length new < 6 -> do
        setMessage (fromString (unpack (t "auth.passwordTooShort")))
        pure Nothing
      new -> pure $ Just new
