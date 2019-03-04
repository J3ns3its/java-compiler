-- {-# LANGUAGE FlexibleInstances #-}
module DirectedGraph(
  DirectedGraph,
  emptyGraph,
  nodes,
  succs,
  predecessors,
  successors,
  addEdge,
  addEdges
  --outDegree,
  
  ) where

import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Jenseits(cross)

data DirectedGraph = DirectedGraph {
    preds :: IntMap [Int],
    succs :: IntMap [Int]
  }

nodes :: DirectedGraph -> IntSet
nodes g = IntSet.union (IntMap.keysSet (preds g)) (IntMap.keysSet (succs g))

predecessors :: DirectedGraph -> Int -> [Int]
predecessors g x = IntMap.findWithDefault [] x (preds g)

successors :: DirectedGraph -> Int -> [Int]
successors g x = IntMap.findWithDefault [] x (succs g)

--outDegree :: DirectedGraph -> Int -> Int
--outDegree g x = IntSet.size $ successors g x

emptyGraph :: DirectedGraph
emptyGraph = DirectedGraph { preds = IntMap.empty, succs = IntMap.empty  }

addEdge :: DirectedGraph -> Int -> Int -> DirectedGraph
addEdge g src dst = g {
   preds = IntMap.insertWith (\[n] old -> n:old) dst [src] (preds g),
   succs = IntMap.insertWith (\[n] old -> n:old) src [dst] (succs g)
   }

addEdges :: DirectedGraph -> Int -> [Int] -> DirectedGraph
addEdges g src dsts = g {
   preds = foldl' (\pmap dst ->
           IntMap.insertWith (\[n] old -> n:old) dst [src] pmap) (preds g) dsts,
   succs = IntMap.insertWith (++) src dsts (succs g)
   }

instance Show DirectedGraph where
  show g =
    "{ preds = " ++ show (map (cross (id, sort)) . IntMap.toAscList $ preds g)
    ++
    ", succs = " ++ show (map (cross (id, sort)) . IntMap.toAscList $ succs g)
    ++ " }"
-- eof

