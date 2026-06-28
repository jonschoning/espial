module Util where

import ClassyPrelude (Text)
import Data.CaseInsensitive qualified as CI
import Data.List (nubBy)
import Data.Map.Strict qualified as M
import Data.Text (replace, words)
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