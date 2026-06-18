module Util where

import Data.Map.Strict qualified as M
import Prelude

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