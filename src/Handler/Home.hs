{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
  maybeAuthUsername >>=
  \case
    Nothing -> redirect $ AuthR LoginR
    Just uname -> redirect $ UserR (UserNameP uname)
