{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Docs where

import Import

getDocsSearchR :: Handler Html
getDocsSearchR = do
  app <- getYesod
  muser <- fmap entityVal <$> maybeAuth
  lang <- getCurrentLang (LangSourceUser muser)
  let t = \key -> appTranslate app lang (I18nKey key)
  popupLayout
    $(widgetFile "docs-search")
