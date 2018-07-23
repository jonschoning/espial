{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

import qualified Data.ByteString.Char8 as B8
import qualified Data.Aeson as A

-- Forms

type MonadHandlerForm m = (RenderMessage App FormMessage, HandlerSite m ~ App, MonadHandler m)

type Form f = Html -> MForm Handler (FormResult f, Widget)

runInputPostJSONResult
  :: (FromJSON a, MonadHandlerForm m)
  => FormInput m a -> m (FormResult a)
runInputPostJSONResult form = do
  mct <- lookupHeader "content-type"
  case fmap (B8.takeWhile (/= ';')) mct of
    Just "application/json" ->
      parseJsonBody >>= \case
        A.Success a -> pure $ FormSuccess a
        A.Error e -> pure $ FormFailure [pack e]
    Just "application/x-www-form-urlencoded" ->
      runInputPostResult form
    _ -> pure FormMissing

runInputPostJSON
  :: (FromJSON a, MonadHandlerForm m)
  => FormInput m a -> m a
runInputPostJSON form =
  runInputPostJSONResult form >>=
  \case
    FormSuccess a -> pure a
    FormFailure e -> invalidArgs e
    FormMissing -> invalidArgs []

class MkIForm a where
  mkIForm :: MonadHandlerForm m => FormInput m a

aFormToMaybeGetSuccess
  :: MonadHandler f
  => AForm f a -> f (Maybe a)
aFormToMaybeGetSuccess =
  fmap maybeSuccess . fmap fst . runFormGet . const . fmap fst . aFormToForm

aFormToMaybePostSuccess
  :: MonadHandlerForm f
  => AForm f a -> f (Maybe a)
aFormToMaybePostSuccess =
  fmap maybeSuccess . fmap fst . runFormPostNoToken . const . fmap fst . aFormToForm

maybeSuccess :: FormResult a -> Maybe a
maybeSuccess (FormSuccess a) = Just a
maybeSuccess _ = Nothing


-- FieldSettings

named :: Text -> FieldSettings master -> FieldSettings master
named n f =
  f
  { fsName = Just n
  , fsId = Just n
  }

attr :: (Text,Text) -> FieldSettings master -> FieldSettings master
attr n f =
  f
  { fsAttrs = n : fsAttrs f
  }

attrs :: [(Text,Text)] -> FieldSettings master -> FieldSettings master
attrs n f =
  f
  { fsAttrs = n ++ fsAttrs f
  }

cls :: [Text] -> FieldSettings master -> FieldSettings master
cls n = attrs [("class", intercalate " " n)]
