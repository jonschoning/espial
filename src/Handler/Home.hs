{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do 
  musername <- maybeAuthUsername
  case musername of
    Nothing -> redirect (AuthR LoginR)
    Just username -> redirect (UserR (UserNameP username))
