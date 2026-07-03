{-# LANGUAGE CPP #-}
module Pretty where

import ClassyPrelude

#ifdef DEVELOPMENT
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Show.Pretty (ppShow)

cpprint :: (MonadIO m, Show a) => a -> m ()
cpprint = putStrLn . pack . hscolour TTY defaultColourPrefs False False "" False . ppShow

cprint :: (MonadIO m, Show a) => a -> m ()
cprint = putStrLn . pack . hscolour TTY defaultColourPrefs False False "" False . show

pprint :: (MonadIO m, Show a) => a -> m ()
pprint = putStrLn . pack . ppShow
#else
cpprint :: (MonadIO m, Show a) => a -> m ()
cpprint _ = pure ()

cprint :: (MonadIO m, Show a) => a -> m ()
cprint _ = pure ()

pprint :: (MonadIO m, Show a) => a -> m ()
pprint _ = pure ()
#endif
