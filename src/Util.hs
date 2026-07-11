module Util where

import ClassyPrelude (Alternative, Text, UTCTime, asum, defaultTimeLocale, formatTime, parseTimeM, toLower)
import Data.CaseInsensitive qualified as CI
import Data.List (nubBy)
import Data.Map.Strict qualified as M
import Data.Text (pack, replace, unpack, words)
import Data.Time qualified as TI (ParseTime)
import Prelude hiding (words)

inverseMap ::
  forall a k.
  (Bounded a, Enum a, Ord k) =>
  (a -> k) ->
  (k -> Maybe a)
inverseMap f = \k -> M.lookup k dict
  where
    dict :: M.Map k a
    dict = M.fromList ((fmap . toFst) f ([minBound .. maxBound]))
{-# INLINE inverseMap #-}

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
{-# INLINE toFst #-}

batchOf :: Int -> [a] -> [[a]]
batchOf _ [] = []
batchOf n xs = let (h, t) = splitAt n xs in h : batchOf n t
{-# INLINE batchOf #-}

normalizeTags :: Text -> [Text]
normalizeTags = nubBy (\a b -> CI.mk a == CI.mk b) . words . replace "," " "

parseTimeText :: (TI.ParseTime t, MonadFail m, Alternative m) => Text -> m t
parseTimeText t =
  asum $
    flip (parseTimeM True defaultTimeLocale) (unpack t)
      <$> [ "%FT%T%Q%Z", -- 2018-12-31T23:59:59.123Z  (iso8601 with timezone)
            "%FT%T%Q", -- 2018-12-31T23:59:59.123   (iso8601 without timezone)
            "%F %T%Q", -- 2018-12-31 23:59:59.123
            "%F %T%Q %Z", -- 2018-12-31 23:59:59.123 UTC
            "%F %T", -- 2018-12-31 23:59:59
            "%F", -- 2018-12-31
            "%-m/%-d/%Y", -- 12/31/2018
            "%s", -- 1535932800
            "%s%Q" -- 1535932800.123
          ]

parseBoolText :: (MonadFail m) => Text -> m Bool
parseBoolText t = case toLower t of
  "true" -> pure True
  "false" -> pure False
  _ -> fail "expected true or false"

format8601z :: UTCTime -> Text
format8601z = pack . formatTime defaultTimeLocale "%FT%T%QZ"