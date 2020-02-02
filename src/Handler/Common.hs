-- | Common handler functions.
module Handler.Common where

import Import

import Data.FileEmbed (embedFile)
import Text.Read
import Data.Aeson as A

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
lookupPagingParams =
  (,)
  <$> getUrlSessionParam "count"
  <*> getUrlParam "page"

getUrlParam :: (Read a) => Text -> Handler (Maybe a)
getUrlParam name = do
  p <- fmap parseMaybe (lookupGetParam name)
  pure p
  where
    parseMaybe x = readMaybe . unpack =<< x

getUrlSessionParam :: forall a.
  (Show a, Read a)
  => Text
  -> Handler (Maybe a)
getUrlSessionParam name = do
  p <- fmap parseMaybe (lookupGetParam name)
  s <- fmap parseMaybe (lookupSession name)
  for_ p (setSession name . (pack . show))
  pure (p <|> s)
  where
    parseMaybe :: Maybe Text -> Maybe a
    parseMaybe x = readMaybe . unpack =<< x

lookupTagCloudMode :: MonadHandler m => m (Maybe TagCloudMode)
lookupTagCloudMode = do
  (A.decode . fromStrict =<<) <$> lookupSessionBS "tagCloudMode"

setTagCloudMode :: MonadHandler m => TagCloudMode -> m ()
setTagCloudMode = setSessionBS "tagCloudMode" . toStrict . A.encode

getTagCloudMode :: MonadHandler m => Bool -> [Tag] -> m TagCloudMode
getTagCloudMode isowner tags = do
  ms <- lookupTagCloudMode
  let expanded = maybe False isExpanded ms
  pure $
    if not isowner
      then TagCloudModeNone
      else if not (null tags)
             then TagCloudModeRelated expanded tags
             else case ms of
                    Nothing -> TagCloudModeTop expanded 200
                    Just (TagCloudModeRelated e _) -> TagCloudModeTop e 200
                    Just m -> m

  
