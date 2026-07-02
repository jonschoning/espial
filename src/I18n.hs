module I18n
  ( loadTranslations,
    translate,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HM
import Import.NoFoundation
import Numeric (showHex)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension)

loadTranslations :: FilePath -> IO (Text, I18nMap)
loadTranslations staticDir = do
  let localesDir = staticDir </> "locales"
  langDirs <- listDirectory localesDir
  (rawss, pairs) <- unzip . catMaybes <$> mapM (loadLang localesDir) langDirs
  let langmap = HM.fromList pairs
  pure (getHash rawss, langmap)
  where
    loadLang :: FilePath -> FilePath -> IO (Maybe ([ByteString], (I18nLang, HashMap I18nNs (HashMap I18nKey Text))))
    loadLang localesDir langDir = do
      let langPath = localesDir </> langDir
      let mlang = toI18nLang langDir
      case mlang of
        Nothing -> pure Nothing
        Just lang -> do
          files <- listDirectory langPath
          let nsFiles = filter ((== ".json") . takeExtension) files
          (raws, nsPairs) <- unzip <$> mapM (loadNs langPath) nsFiles
          pure $ Just (raws, (lang, HM.fromList nsPairs))

    loadNs :: FilePath -> FilePath -> IO (ByteString, (I18nNs, HashMap I18nKey Text))
    loadNs langPath nsFile = do
      let path = langPath </> nsFile
      rawNs <- readFile path
      let ns = I18nNs (pack (takeBaseName nsFile))
          keyMap = maybe HM.empty (flattenValue "") (A.decodeStrict' rawNs)
      pure (rawNs, (ns, keyMap))

    flattenValue :: Text -> A.Value -> HashMap I18nKey Text
    flattenValue prefix = \case
      A.String s -> HM.singleton (I18nKey prefix) s
      A.Object o ->
        KM.foldrWithKey
          ( \k v acc ->
              let key = if null prefix then Key.toText k else prefix <> "." <> Key.toText k
               in union (flattenValue key v) acc
          )
          HM.empty
          o
      _ -> HM.empty

    getHash :: [[ByteString]] -> Text
    getHash bs = pack $ take 7 $ concatMap byteHex $ unpack (SHA256.hash (concat (concat bs)))
      where
        byteHex :: Word8 -> String
        byteHex b = let s = showHex b "" in if length s == 1 then '0' : s else s

translate :: I18nMap -> I18nLang -> I18nNs -> I18nKey -> Text
translate trans lang ns key =
  case lookup lang trans of
    Nothing -> unI18nKey key
    Just nsmap -> case lookup ns nsmap of
      Nothing -> unI18nKey key
      Just keymap -> findWithDefault (unI18nKey key) key keymap
