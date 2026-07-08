module Handler.AccountSettings where

import ClassyPrelude.Yesod qualified as CP
import Data.Aeson qualified as A
import Import
import Model.File

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

-- API key is only returned in plaintext here, since it is stored hashed.
postApiKeyR :: Handler Value
postApiKeyR = do
  userId <- requireAuthId
  apiKey <- liftIO generateApiKey
  runDB (update userId [UserApiToken CP.=. Just (hashApiKey apiKey)])
  pure (A.object ["apiKey" A..= unApiKey apiKey])

deleteApiKeyR :: Handler Value
deleteApiKeyR = do
  userId <- requireAuthId
  runDB (update userId [UserApiToken CP.=. Nothing])
  pure (A.object ["ok" A..= True])

getExportBookmarksR :: Handler TypedContent
getExportBookmarksR = do
  userId <- requireAuthId
  fmarks <- runDB (getFileBookmarks userId)
  downloadAttachment typeJson "espial-bookmarks.json" (A.encode fmarks)

getExportNetscapeR :: Handler TypedContent
getExportNetscapeR = do
  userId <- requireAuthId
  nbmarks <- runDB (getNetscapeBookmarks userId)
  downloadAttachment typeHtml "espial-bookmarks.html" (renderNetscapeBookmarks nbmarks)

getExportNotesR :: Handler TypedContent
getExportNotesR = do
  userId <- requireAuthId
  fnotes <- runDB (getFileNotes userId)
  downloadAttachment typeJson "espial-notes.json" (A.encode fnotes)

postImportBookmarksR :: Handler ()
postImportBookmarksR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case A.eitherDecode' body of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right (fmarks :: [FileBookmark]) -> respondImported =<< runDB (insertFileBookmarks userId fmarks)

postImportFirefoxR :: Handler ()
postImportFirefoxR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case A.eitherDecode' body of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right (node :: FirefoxBookmarkNode) -> respondImported =<< runDB (insertFirefoxBookmarks userId node)

postImportNetscapeR :: Handler ()
postImportNetscapeR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case parseNetscapeBookmarks (decodeUtf8 (toStrict body)) of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right nbmarks -> respondImported =<< runDB (insertNetscapeBookmarks userId nbmarks)

postImportNotesR :: Handler ()
postImportNotesR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case A.eitherDecode' body of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right (fnotes :: [FileNote]) -> respondImported =<< runDB (insertFileNotes userId fnotes)

downloadAttachment :: (ToContent a) => ContentType -> Text -> a -> Handler TypedContent
downloadAttachment ct filename body = do
  addHeader "Content-Disposition" ("attachment; filename=\"" <> filename <> "\"")
  pure (TypedContent ct (toContent body))

importBodyLbs :: Handler LByteString
importBodyLbs = runConduit (rawRequestBody .| sinkLazy)

respondImported :: Int -> Handler a
respondImported n = sendStatusJSON ok200 (A.object ["imported" A..= n])

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
  let hashAlgo = appPasswordHashConfig (appSettings app)
  runInputPostResult ((,) <$> ireq textField "oldpassword" <*> ireq textField "newpassword") >>= \case
    FormSuccess (old, new) -> do
      runDB (authenticatePassword hashAlgo (userName user) old) >>= \case
        Nothing -> setMessage (toHtml (t "auth.incorrectOldPassword"))
        Just _ ->
          validateNewPassword t new >>= \case
            Just newValid -> do
              newHash <- liftIO (hashPasswordWith hashAlgo newValid)
              void $ runDB (update userId [UserPasswordHash CP.=. newHash])
              setMessage (toHtml (t "auth.passUpdated"))
            _ -> pure ()
    _ -> setMessage (toHtml (t "auth.missingRequiredFields"))
  redirect ChangePasswordR
  where
    validateNewPassword :: (Text -> Text) -> Text -> Handler (Maybe Text)
    validateNewPassword t = \case
      new | length new < 6 -> do
        setMessage (toHtml (t "auth.passwordTooShort"))
        pure Nothing
      new -> pure $ Just new
