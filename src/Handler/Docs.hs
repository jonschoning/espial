{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Docs where

import Import

getDocsSearchR :: Handler Html
getDocsSearchR = popupLayout $
  $(widgetFile "docs-search")
