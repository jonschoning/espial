module Handler.Locales (getLocalesFileR) where

import Import

-- | Serve a locale JSON file, ignoring the cache-busting hash segment.
getLocalesFileR :: Text -> Texts -> Handler TypedContent
getLocalesFileR _hash pieces = do
  app <- getYesod
  let staticDir = appStaticDir (appSettings app)
      path = foldl' (</>) (staticDir </> "locales") (map unpack pieces)
  sendFile typeJson path
