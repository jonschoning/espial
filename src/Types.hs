module Types where

import ClassyPrelude.Yesod

type DBM m a = MonadUnliftIO m => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

