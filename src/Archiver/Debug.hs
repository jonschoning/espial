module Archiver.Debug
  ( debugArchiverBackend,
  )
where

import Archiver.Backend
import ClassyPrelude
import Control.Monad.Logger (logDebug, runLoggingT)
import Model (Url (..))
import Yesod.Default.Main (LogFunc)

-- | Debug backend.
debugArchiverBackend :: LogFunc -> ArchiverBackend
debugArchiverBackend logFunc =
  ArchiverBackend
    { runArchiver = \uid bid url -> flip runLoggingT logFunc $ do
        $(logDebug) $ "Debug archiver: would archive URL " <> unUrl url <> " for bookmark ID " <> tshow bid <> " and user ID " <> tshow uid,
      isUrlDenylisted = const False
    }