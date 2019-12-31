module TemporalGraph.Graph where

import Definitions

import WeightedGraph.Graph

import Data.List

type Time = Int
type Length = Int

type Interval = (Time, Length)
type TemporalGraph = WeightedGraph Interval
type TemporalEdge = WeightedEdge Interval

type TimeInterval = (Time, Time)

compareWeightedEdges :: TemporalEdge -> TemporalEdge -> Ordering
compareWeightedEdges (_, _, (t1, _)) (_, _, (t2, _)) | t1 < t2 = LT
    | otherwise = GT

getEdgeStream :: TemporalGraph -> [TemporalEdge]
getEdgeStream g = sortBy compareWeightedEdges (weightedEdges g)

shrinkByGreaterValue :: Time -> TemporalEdge -> [TemporalEdge] -> [TemporalEdge]
shrinkByGreaterValue t' e ls = let (_, _, (t, w)) = e in if (t + w) > t' then ls else (e:ls)

shrinkByLesserValue :: Time -> TemporalEdge -> [TemporalEdge] -> [TemporalEdge]
shrinkByLesserValue t' e ls = let (_, _, (t, w)) = e in if t < t' then ls else (e:ls)

shrinkByInterval :: TimeInterval -> TemporalEdge -> [TemporalEdge] -> [TemporalEdge]
shrinkByInterval (t1, t2) e ls = let (_, _, (t, w)) = e in if t > t1 && (t + w) < t2 then (e:ls) else ls 

trimByGreaterValue :: [TemporalEdge] -> Time -> [TemporalEdge]
trimByGreaterValue e t' = foldr (shrinkByGreaterValue t') [] e

trimByLesserValue :: [TemporalEdge] -> Time -> [TemporalEdge]
trimByLesserValue e t' = foldr (shrinkByLesserValue t') [] e

trimByInterval :: [TemporalEdge] -> TimeInterval -> [TemporalEdge]
trimByInterval e i = foldr (shrinkByInterval i) [] e
