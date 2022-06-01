module Handler.AccountSettings where

import Import
import qualified ClassyPrelude.Yesod as CP

getAccountSettingsR :: Handler Html
getAccountSettingsR = do
  (_, user) <- requireAuthPair
  let accountSettingsEl = "accountSettings" :: Text
  let accountSettings = toAccountSettingsForm user
  defaultLayout do
    $(widgetFile "user-settings")
    toWidgetBody [julius|
        app.userR = "@{UserR (UserNameP $ userName user)}";
        app.dat.accountSettings = #{ toJSON accountSettings } || []; 
    |]
    toWidget [julius|
      PS.renderAccountSettings('##{rawJS accountSettingsEl}')(app.dat.accountSettings)();
    |]

postEditAccountSettingsR :: Handler ()
postEditAccountSettingsR = do
  userId <- requireAuthId
  accountSettingsForm <- requireCheckJsonBody
  runDB (updateUserFromAccountSettingsForm userId accountSettingsForm)


getChangePasswordR :: Handler Html
getChangePasswordR = do
  void requireAuthId
  req <- getRequest
  defaultLayout $
    $(widgetFile "change-password")

postChangePasswordR :: Handler Html
postChangePasswordR = do
  (userId, user) <- requireAuthPair
  runInputPostResult ((,) <$> ireq textField "oldpassword" <*> ireq textField "newpassword") >>= \case
    FormSuccess (old, new) -> do
      runDB (authenticatePassword (userName user) old) >>= \case
        Nothing -> setMessage "Incorrect Old Password"
        Just _ -> validateNewPassword new >>= \case
          Just newValid -> do
            newHash <- liftIO (hashPassword newValid)
            void $ runDB (update userId [UserPasswordHash CP.=. newHash])
            setMessage "Password Changed Successfully"
          _ -> pure ()
    _ -> setMessage "Missing Required Fields"
  redirect ChangePasswordR

validateNewPassword :: Text -> Handler (Maybe Text)
validateNewPassword = \case
    new | length new < 6 -> do
          setMessage "Password must be at least 6 characters long"
          pure Nothing
    new -> pure $ Just new
