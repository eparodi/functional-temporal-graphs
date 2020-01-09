module TemporalGraph.PathAlgorithms where

import Definitions

import WeightedGraph.Graph
import TemporalGraph.Graph

import Data.List

import Data.Array
import Data.Array.ST
import Control.Monad.ST

type TransformedGraph = WeightedGraph Time
type TimeEdgeNode = EdgeNode Time
type TimeEdge = WeightedEdge Time

mapV :: (EdgeNode Interval -> Time) -> Index -> [EdgeNode Interval] -> [Time]
mapV f _ intervals = rmdup (sort (map f intervals))

getIn :: EdgeNode Interval -> Time
getIn (_, (t, _)) = t

getOut :: EdgeNode Interval -> Time
getOut (_, (t, l)) = t + l

rmdup :: Eq a => [a] -> [a]
rmdup xs = rmdup' [] xs
  where
    rmdup' acc [] = []
    rmdup' acc (x:xs)
      | x `elem` acc = rmdup' acc xs
      | otherwise    = x : rmdup' (x:acc) xs

vIn :: TemporalGraph -> [(Index, [Time])]
vIn g  = mapToList (mapV getOut) (transposeWG g)

vOut :: TemporalGraph -> [(Index, [Time])]
vOut = mapToList (mapV getIn)

numerateList :: [Int] -> Index -> [(Index, Int)]
numerateList [] i = []
numerateList (x:xs) i = (i, x) : (numerateList xs (i+1))

numerateListOfLists :: [(Index, [Time])] -> Index -> [(Index, [(Index, Time)])]
numerateListOfLists [] i = []
numerateListOfLists ((idx, x):xs) i = (idx, (numerateList x i)) : (numerateListOfLists xs (i + length x))

boundsList :: [(Index, [Time])] -> Index
boundsList [] = 0
boundsList ((idx, x):xs) = (length x) + boundsList xs

getEdgesFromTimeList :: [(Index, Time)] -> [TimeEdge]
getEdgesFromTimeList [] = []
getEdgesFromTimeList [_] = []
getEdgesFromTimeList [(id1, _), (id2, _)] = [(id1, id2, 0)]
getEdgesFromTimeList ((id1, _):xs) = 
    let (id2, _) = head xs
        in (id1, id2, 0) : (getEdgesFromTimeList xs)

getEdgesFromNodeList :: [(Index, [(Index, Time)])] -> [TimeEdge]
getEdgesFromNodeList [] = []
getEdgesFromNodeList ((idx, x):xs) = (getEdgesFromTimeList x) ++ (getEdgesFromNodeList xs)

getEdgesFromTimeLists :: [(Index, Time)] -> [(Index, Time)] -> [TimeEdge]
getEdgesFromTimeLists [] [] = []
getEdgesFromTimeLists [] _ = []
getEdgesFromTimeLists _ [] = []
getEdgesFromTimeLists ((idin, tin):xs) ((idout, tout):ys)
    | tout > tin = (idin, idout, 0) : (getEdgesFromTimeLists xs ys)
    | otherwise = getEdgesFromTimeLists xs ((idout, tout):ys)

getEdgesFromNodeLists :: [(Index, [(Index, Time)])] -> [(Index, [(Index, Time)])] -> [TimeEdge]
getEdgesFromNodeLists [] [] = []
getEdgesFromNodeLists ((idx, x):xs) ((idy, y):ys) = (getEdgesFromTimeLists x y) ++ (getEdgesFromNodeLists xs ys)

getEdgesFromEdgesAndArrs :: Table [(Index, Time)] -> Table [(Index, Time)] -> [TemporalEdge] -> [TimeEdge]
getEdgesFromEdgesAndArrs vin vout [] = []
getEdgesFromEdgesAndArrs vin vout ((u, v, interval):xs) = (f (vin!v) (vout!u) interval) : (getEdgesFromEdgesAndArrs vin vout xs)
    where f :: [(Index, Time)] -> [(Index, Time)] -> TimeInterval -> TimeEdge
          f ((ii, ti):vin) ((io, to):vout) (t, l) 
            | ti == (t + l) && to == t = (ii, io, l)
            | ti /= (t + l) && to == t = f vin ((io, to):vout) (t, l) 
            | ti == (t + l) && to /= t = f ((ii, ti):vin) vout (t, l)
            | otherwise = f vin vout (t, l)

getTransformedEdges :: TemporalGraph -> [TimeEdge]
getTransformedEdges g = 
    let vin = numerateListOfLists (vIn g) 1
        vout = numerateListOfLists (vOut g) ((boundsList (vIn g)) + 1)
        vinArr = array (bounds g) vin
        voutArr = array (bounds g) vout
        in (getEdgesFromNodeList vin) ++ (getEdgesFromNodeList vout) ++ (getEdgesFromNodeLists vin vout) ++ (getEdgesFromEdgesAndArrs vinArr voutArr (weightedEdges g))