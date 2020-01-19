module TemporalGraph.PathAlgorithms where

import Definitions

import WeightedGraph.Graph
import TemporalGraph.Graph
import Tree

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

boundsList :: [(Index, [a])] -> Index
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
getEdgesFromTimeLists ((idout, tout):os) ((idin, tin):is)
    | tin <= tout = (idin, idout, 0) : (getEdgesFromTimeLists os ((idin, tin):is))
    | otherwise = getEdgesFromTimeLists ((idout, tout):os) is
    -- let (_, tinh) = (head is) in
    --     if tout > tin && tout < tinh then (idin, idout, 0) : (getEdgesFromTimeLists os is)
    --     else getEdgesFromTimeLists ((idout, tout):os) is

getEdgesFromNodeLists :: [(Index, [(Index, Time)])] -> [(Index, [(Index, Time)])] -> [TimeEdge]
getEdgesFromNodeLists [] [] = []
getEdgesFromNodeLists ((ido, o):os) ((idi, i):is) = (getEdgesFromTimeLists (reverse o) (reverse i)) ++ (getEdgesFromNodeLists os is)

getEdgesFromEdgesAndArrs :: Table [(Index, Time)] -> Table [(Index, Time)] -> [TemporalEdge] -> [TimeEdge]
getEdgesFromEdgesAndArrs vin vout [] = []
getEdgesFromEdgesAndArrs vin vout ((u, v, interval):xs) = (f (vin!v) (vout!u) interval) : (getEdgesFromEdgesAndArrs vin vout xs)
    where f :: [(Index, Time)] -> [(Index, Time)] -> TimeInterval -> TimeEdge
          f ((ii, ti):vin) ((io, to):vout) (t, l) 
            | ti == (t + l) && to == t = (io, ii, l)
            | ti /= (t + l) && to == t = f vin ((io, to):vout) (t, l) 
            | ti == (t + l) && to /= t = f ((ii, ti):vin) vout (t, l)
            | otherwise = f vin vout (t, l)

getInitialEdges :: Index -> Table [(Index, Time)] -> [TimeEdge]
getInitialEdges idx vout = map f (vout!idx)
    where f :: (Index, Time) -> TimeEdge
          f (idx, t) = (1, idx, 0)

toIndexArray :: [(Index, [(Index, Time)])] -> Bounds -> Table (Index, Time)
toIndexArray vs bnds = array bnds (foldr f [] vs)
    where f :: (Index, [(Index, Time)]) -> [(Index, (Index, Time))] -> [(Index, (Index, Time))]
          f (idx, a) b = (map (g idx) a) ++ b
            where g :: Index -> (Index, Time) -> (Index, (Index, Time))
                  g idx (i, t) = (i, (idx, t))

getTransformedGraph :: TemporalGraph -> Index -> (WeightedGraph Time, Table (Index, Time))
getTransformedGraph g v = 
    let vin = numerateListOfLists (vIn g) 2
        vout = numerateListOfLists (vOut g) ((boundsList vin) + 2)
        bnds = (1, (boundsList vin) + (boundsList vout) + 1)
        vinArr = array (bounds g) vin
        voutArr = array (bounds g) vout
        in (
            buildWG bnds 
            (
                (getEdgesFromNodeList vin) ++ 
                (getEdgesFromNodeList vout) ++
                (getEdgesFromNodeLists vout vin) ++
                (getEdgesFromEdgesAndArrs vinArr voutArr (weightedEdges g)) ++
                (getInitialEdges v voutArr)
            ),
            toIndexArray ([(v, [(1, 0)])] ++ vin ++ vout) bnds
        )

generateTimeTree :: WeightedGraph Time -> Index -> Tree (Index, Time)
generateTimeTree g v = Node (v, 0) (map (generateAux g) (g!v))
    where generateAux :: WeightedGraph Time -> (Index, Time) -> Tree (Index, Time)
          generateAux g (idx, val) = Node (idx, val) (map (generateAux g) (g!idx))

getListOfTGNodes :: Table (Index, Time) -> [Index] -> [Index]
getListOfTGNodes n [x] =
    let (id, _) = n!x
    in [id]
getListOfTGNodes n (x:(y:xs)) =
    let (id1, _) = n!x
        (id2, _) = n!y
    in
        if id1 == id2 then getListOfTGNodes n (y:xs)
        else id1 : (getListOfTGNodes n (y:xs))

pathsToNode :: Index -> Table (Index, Time) -> Tree (Index, Time) -> [[Index]]
pathsToNode x nodes (Node (idx, t) ns) = 
    let (y, _) = nodes!idx in [[idx] | x == y] ++ map (idx:) (pathsToNode x nodes =<< ns)

getFunctionPath :: (Table (Index, Time) -> [Index] -> [Index] -> [Index]) -> Table (Index, Time) -> [[Index]] -> [Index]
getFunctionPath f nodes [] = []
getFunctionPath f nodes [x] = x
getFunctionPath f nodes [x, y] = f nodes x y
getFunctionPath f nodes (x:xs) = f nodes x (getFunctionPath f nodes xs)

shortestPathF :: Table (Index, Time) -> [Index] -> [Index] -> [Index]
shortestPathF n x y 
    | length (getListOfTGNodes n x) > length (getListOfTGNodes n y) = y
    | otherwise = x

earliestPathF :: Table (Index, Time) -> [Index] -> [Index] -> [Index]
earliestPathF n x y =
    let (_, t1) = n!(last x)
        (_, t2) = n!(last y)
    in
        if t1 <= t2 then x
        else y

latestPathF :: Table (Index, Time) -> [Index] -> [Index] -> [Index]
latestPathF n x y =
    let (_, t1) = n!(x!!1)
        (_, t2) = n!(y!!1)
    in
        if t1 >= t2 then x
        else y

fastestPathF :: Table (Index, Time) -> [Index] -> [Index] -> [Index]
fastestPathF n x y =
    let (_, d1) = n!(x!!1)
        (_, d2) = n!(y!!1)
        (_, a1) = n!(last x)
        (_, a2) = n!(last y)
    in
        if (a1 - d1) <= (a2 - d2) then x
        else y

fPath :: (Table (Index, Time) -> [Index] -> [Index] -> [Index]) -> TemporalGraph ->  Index -> Index -> ([Index], Maybe TimeInterval)
fPath f tg si fi = 
    let (g, n) = getTransformedGraph tg si
        in translatePathToNodesAndGetInterval n (getFunctionPath f n (
                pathsToNode fi n (
                    generateTimeTree g 1)))

shortestPath :: TemporalGraph ->  Index -> Index -> ([Index], Maybe TimeInterval)
shortestPath = fPath shortestPathF

earliestArrivalPath :: TemporalGraph ->  Index -> Index -> ([Index], Maybe TimeInterval)
earliestArrivalPath = fPath earliestPathF

latestDeparturePath :: TemporalGraph ->  Index -> Index -> ([Index], Maybe TimeInterval)
latestDeparturePath = fPath latestPathF

fastestPath :: TemporalGraph ->  Index -> Index -> ([Index], Maybe TimeInterval)
fastestPath = fPath fastestPathF

translatePathToNodesAndGetInterval :: Table (Index, Time) -> [Index] -> ([Index], Maybe TimeInterval)
translatePathToNodesAndGetInterval n xs = (foldr (f n) [] xs, getInterval n xs)
    where f :: Table (Index, Time) -> Index -> [Index] -> [Index]
          f n x [] = let (idx, _) = n!x in [idx]
          f n x (y:ys) = let (idx, _) = n!x in
              if idx == y then (y:ys)
              else (idx:(y:ys))
          getInterval :: Table (Index, Time) -> [Index] -> Maybe TimeInterval
          getInterval _ [] = Nothing
          getInterval n xs =
              let (_, st) = n!(xs !! 1)
                  (_, ft) = n!(last xs)
                  in Just (st, ft)
                  
