{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module I18nSpec (spec) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import I18n (loadTranslations, translate')
import TestImport

-- | Dotted key paths for every string leaf in a JSON value, mirroring the
-- flattening done by I18n.loadTranslations so the discovered keys match what
-- `translate` actually looks up.
collectKeys :: Text -> A.Value -> [Text]
collectKeys prefix = \case
  A.String _ -> [prefix]
  A.Object o -> concatMap (\(k, v) -> collectKeys (joinKey prefix (Key.toText k)) v) (KM.toList o)
  _ -> []
  where
    joinKey p k = if null p then k else p <> "." <> k

spec :: Spec
spec = describe "I18n translations" $
  it "every key discovered from en/translation.json resolves to a translation for every I18nLang" $ do
    enJson <- BS.readFile "static/locales/en/translation.json"
    enValue <- case A.decodeStrict' enJson of
      Nothing -> expectationFailure "failed to parse static/locales/en/translation.json" >> pure A.Null
      Just v -> pure v
    let enKeys = collectKeys "" enValue
    enKeys `shouldSatisfy` (not . null)

    (_, translations) <- loadTranslations "static"

    let ns = I18nNs "translation"
        
        misses =
          [ (lang, key)
            | lang <- [minBound .. maxBound],
              key <- enKeys,
              let translated = translate' translations lang ns (I18nKey key)
              in maybe True null translated
          ]

    misses `shouldBe` []
