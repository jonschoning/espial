{-# LANGUAGE TupleSections #-}

module GraphUtils
  ( getPathValues,
  )
where

import ClassyPrelude.Yesod
import Data.Graph.Inductive (Gr, mkGraph, sp)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntSet as IntSet

type Node = Int

type Edge = (Int, Int)

getPathValues :: [(Edge, a)] -> Node -> Node -> Maybe [a]
getPathValues edgeData start end = map (edgeMap !) . nodesToEdges <$> sp start end graph
  where
    graph = mkGraph' $ map fst edgeData
    nodesToEdges nodes = zip nodes $ maybe [] tail $ fromNullable nodes
    edgeMap = HashMap.fromList edgeData

mkGraph' :: [Edge] -> Gr () Int
mkGraph' edgeData = mkGraph nodes edges
  where
    detuple (a, b) = [a, b]
    nodes = map (,()) . IntSet.toList . IntSet.fromList . concatMap detuple $ edgeData
    edges = map (uncurry (,,1)) edgeData