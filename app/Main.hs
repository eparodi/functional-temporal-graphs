module Main where

import SimpleGraph.Graph
import WeightedGraph.Graph
import TemporalGraph.Graph

-- graph = buildG (1, 10) [(1, 10), (1, 7),(2, 9),(2, 1),(3, 8),(3, 5),(5, 10),(5, 8),(5, 4),(6, 9),(7, 6),(7, 2)]
-- temporalGraph = buildWG (1, 10) [
--     (1, 10, (2, 1)),
--     (1, 7, (4, 1)),
--     (2, 9, (8, 1)),
--     (2, 1, (3, 1)),
--     (3, 8, (7, 1)),
--     (3, 5, (6, 1)),
--     (5, 10, (5, 1)),
--     (5, 8, (0, 1)),
--     (5, 4, (3, 1)),
--     (6, 9, (6, 1)),
--     (7, 6, (4, 1)),
--     (7, 2, (9, 1))]


import Control.Monad.ST
import Data.Array.ST

buildPair = do arr <- newArray (1,10) 37 :: ST s (STArray s Int Int)
               a <- readArray arr 1
               writeArray arr 1 64
               b <- readArray arr 1
               return (a,b)

main :: IO ()
main = print $ runST buildPair

-- main = print (getEdgeStream (weightedEdges temporalGraph))
