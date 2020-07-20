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
      PS['Main'].renderAccountSettings('##{rawJS accountSettingsEl}')(app.dat.accountSettings)();
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
  userId <- requireAuthId
  mauthuname <- maybeAuthUsername
  mresult <- runInputPostResult ((,) <$> ireq textField "oldpassword" <*> ireq textField "newpassword")
  case (mauthuname, mresult) of
    (Just uname, FormSuccess (old, new)) -> do
      muser <- runDB (authenticatePassword uname old)
      case muser of
        Just _ -> do 
          new' <- liftIO (hashPassword new)
          void $ runDB (update userId [UserPasswordHash CP.=. new'])
          setMessage "Password Changed Successfully"
        _ -> setMessage "Incorrect Old Password"
    _ -> setMessage "Missing Required Fields"
  redirect ChangePasswordR
