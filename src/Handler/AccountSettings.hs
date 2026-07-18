module Handler.AccountSettings where

import ClassyPrelude.Yesod qualified as CP
import Data.Aeson qualified as A
import Import
import Model.File

getAccountSettingsR :: Handler Html
getAccountSettingsR = do
  app <- getYesod
  (_, user) <- requireAuthPair
  lang <- getCurrentLang (LangSourceUser (Just user))
  let frontendBundleName = appFrontendBundleName app
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

getAccountSettingsTabR :: SettingsTabP -> Handler Html
getAccountSettingsTabR _ = getAccountSettingsR

postEditAccountSettingsR :: Handler ()
postEditAccountSettingsR = do
  app <- getYesod
  userId <- requireAuthId
  accountSettingsForm <- requireCheckJsonBody
  runDBWrite (updateUserFromAccountSettingsForm userId accountSettingsForm)
  case _language accountSettingsForm of
    Nothing -> setLanguage (fromI18nLang (appLanguageDefault (appSettings app)))
    Just lang -> setLanguage (fromI18nLang lang)

-- API key is only returned in plaintext here, since it is stored hashed.
postApiKeyR :: Handler Value
postApiKeyR = do
  userId <- requireAuthId
  apiKey <- liftIO generateApiKey
  runDBWrite (update userId [UserApiToken CP.=. Just (hashApiKey apiKey)])
  pure (A.object ["apiKey" A..= unApiKey apiKey])

deleteApiKeyR :: Handler Value
deleteApiKeyR = do
  userId <- requireAuthId
  runDBWrite (update userId [UserApiToken CP.=. Nothing])
  pure (A.object ["ok" A..= True])

getExportBookmarksJsonR :: Handler TypedContent
getExportBookmarksJsonR = do
  userId <- requireAuthId
  fmarks <- runDB (getFileBookmarks userId)
  filename <- exportFilename "espial-bookmarks" "json"
  downloadAttachment typeJson filename (A.encode fmarks)

getExportBookmarksNetscapeR :: Handler TypedContent
getExportBookmarksNetscapeR = do
  userId <- requireAuthId
  nbmarks <- runDB (getNetscapeBookmarks userId)
  filename <- exportFilename "espial-bookmarks" "html"
  downloadAttachment typeHtml filename (renderNetscapeBookmarks nbmarks)

getExportNotesJsonR :: Handler TypedContent
getExportNotesJsonR = do
  userId <- requireAuthId
  fnotes <- runDB (getFileNotes userId)
  filename <- exportFilename "espial-notes" "json"
  downloadAttachment typeJson filename (A.encode fnotes)

postImportBookmarksJsonR :: Handler ()
postImportBookmarksJsonR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case A.eitherDecode' body of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right (fmarks :: [FileBookmark]) -> respondImported =<< runDBWrite (insertFileBookmarks userId fmarks)

postImportBookmarksFirefoxR :: Handler ()
postImportBookmarksFirefoxR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case A.eitherDecode' body of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right (node :: FirefoxBookmarkNode) -> respondImported =<< runDBWrite (insertFirefoxBookmarks userId node)

postImportBookmarksNetscapeR :: Handler ()
postImportBookmarksNetscapeR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case parseNetscapeBookmarks (decodeUtf8 (toStrict body)) of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right nbmarks -> respondImported =<< runDBWrite (insertNetscapeBookmarks userId nbmarks)

postImportNotesJsonR :: Handler ()
postImportNotesJsonR = do
  userId <- requireAuthId
  body <- importBodyLbs
  case A.eitherDecode' body of
    Left e -> sendResponseStatus status400 (pack e :: Text)
    Right (fnotes :: [FileNote]) -> respondImported =<< runDBWrite (insertFileNotes userId fnotes)

downloadAttachment :: (ToContent a) => ContentType -> Text -> a -> Handler TypedContent
downloadAttachment ct filename body = do
  addHeader "Content-Disposition" ("attachment; filename=\"" <> filename <> "\"")
  pure (TypedContent ct (toContent body))

exportFilename :: Text -> Text -> Handler Text
exportFilename base ext = do
  now <- liftIO getCurrentTime
  pure (base <> "_" <> pack (formatTime defaultTimeLocale "%Y-%m-%d_%H_%MZ" now) <> "." <> ext)

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
  lang <- getCurrentLang (LangSourceUser (Just user))
  let t = \key -> appTranslate app lang (I18nKey key)
  -- demoMode = appDemoMode (appSettings app)
  defaultLayout
    $ $(widgetFile "change-password")

postChangePasswordR :: Handler Html
postChangePasswordR = do
  app <- getYesod
  (userId, user) <- requireAuthPair
  lang <- getCurrentLang (LangSourceUser (Just user))
  let t = \key -> appTranslate app lang (I18nKey key)
  when (appDemoMode (appSettings app)) $ do
    setMessage (toHtml (t "changePassword.demoMode"))
    redirect ChangePasswordR
  let hashAlgo = appPasswordHashConfig (appSettings app)
  runInputPostResult ((,) <$> ireq textField "oldpassword" <*> ireq textField "newpassword") >>= \case
    FormSuccess (old, new) -> do
      runDBWrite (authenticatePassword hashAlgo (userName user) old) >>= \case
        Nothing -> setMessage (toHtml (t "auth.incorrectOldPassword"))
        Just _ ->
          validateNewPassword t new >>= \case
            Just newValid -> do
              newHash <- liftIO (hashPasswordWith hashAlgo newValid)
              void $ runDBWrite (update userId [UserPasswordHash CP.=. newHash])
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
