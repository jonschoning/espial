-- | Common handler functions.
module Handler.Common where

import Import

import Data.FileEmbed (embedFile)
import Text.Read

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 5
                 --cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


lookupPagingParams :: Handler (Maybe Int64, Maybe Int64)
lookupPagingParams = do
  cq <- fmap parseMaybe (lookupGetParam "count")
  cs <- fmap parseMaybe (lookupSession "count")
  for_ cq (setSession "count" . (pack . show)) 
  pq <- fmap parseMaybe (lookupGetParam "page")
  pure (cq <|> cs, pq)
  where
    parseMaybe x = readMaybe . unpack =<< x
