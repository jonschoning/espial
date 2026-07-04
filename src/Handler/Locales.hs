module Handler.Locales (getLocalesFileR, isSafePiece, isWithinDir) where

import Data.Char (isAlphaNum)
import Import
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (pathSeparator)

-- | Serve a locale JSON file, ignoring the cache-busting hash segment.
getLocalesFileR :: Text -> Texts -> Handler TypedContent
getLocalesFileR _hash pieces = do
  unless (not (null pieces) && all isSafePiece pieces) notFound
  app <- getYesod
  let staticDir = appStaticDir (appSettings app)
      localesDir = staticDir </> "locales"
      path = foldl' (</>) localesDir (map unpack pieces)
  safe <- liftIO $ isWithinDir localesDir path
  unless safe notFound
  sendFile typeJson path

isWithinDir :: FilePath -> FilePath -> IO Bool
isWithinDir dir path = do
  isFile <- doesFileExist path
  if not isFile
    then pure False
    else do
      canonDir <- canonicalizePath dir
      canonPath <- canonicalizePath path
      pure ((canonDir <> [pathSeparator]) `isPrefixOf` canonPath)

isSafePiece :: Text -> Bool
isSafePiece t = not (null t) && headMay t /= Just '.' && all isSafeChar t
  where
    isSafeChar :: Char -> Bool
    isSafeChar c = isAlphaNum c || c `elem` ("-_." :: String)
