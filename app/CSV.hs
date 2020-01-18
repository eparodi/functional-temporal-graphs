module CSV where

import Definitions
import WeightedGraph.Graph
import TemporalGraph.Graph

import Text.CSV
import Text.Parsec.Error

import Data.Array

toInt :: String -> Int                               
toInt = read

toTemporalEdges :: [Record] -> [TemporalEdge]
toTemporalEdges xs = map f xs
    where f :: Record -> TemporalEdge
          f [v1, v2, t, l] = (toInt v1, toInt v2, (toInt t, toInt l))

getBounds :: [TemporalEdge] -> Bounds
getBounds xs = (1, foldr f 0 xs)
    where f :: TemporalEdge -> Index -> Index
          f (v1, v2, _) z 
            | v1 > z = v1
            | v2 > z = v2
            | otherwise = z

handleError :: ParseError -> Maybe TemporalGraph
handleError _ = Nothing

doWork :: CSV -> Maybe TemporalGraph
doWork csv = Just (buildWG (getBounds (toTemporalEdges csv)) (toTemporalEdges csv))

getGraph :: String -> String -> Maybe TemporalGraph
getGraph filename csv = 
    let output = parseCSV filename csv
    in either handleError doWork output
