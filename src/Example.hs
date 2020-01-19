module Example where

import TemporalGraph.Graph
import TemporalGraph.PathAlgorithms
import WeightedGraph.Graph

import Definitions
import CSV
import Tree

readCSVAndExecutePath :: String -> Time -> Time -> 
    (TemporalGraph ->  Index -> Index -> ([Index], Maybe TimeInterval)) -> IO ([Index], Maybe TimeInterval)
readCSVAndExecutePath filename from to f =
    do
        content <- readFile filename
        let graph = (getGraph filename content) in
            return (executeFunc graph)
        where executeFunc Nothing = ([], Nothing)
              executeFunc (Just g) = (f g from to)

readCSVAndGetTimeTree :: String -> Index -> IO (Tree (Index, Time))
readCSVAndGetTimeTree filename idx = 
    do
        content <- readFile filename
        let graph = (getGraph filename content) in
            return (executeFunc graph)
        where executeFunc Nothing = (Node (idx,0) [])
              executeFunc (Just tg) = 
                let (g, n) = getTransformedGraph tg idx in
                    generateTimeTree g 1

readCSVAndGetNodePaths :: String -> Index -> Index -> IO ([[Index]])
readCSVAndGetNodePaths filename from to = 
    do
        content <- readFile filename
        let graph = (getGraph filename content) in
            return (executeFunc graph)
        where executeFunc Nothing = []
              executeFunc (Just tg) = 
                let (g, n) = getTransformedGraph tg from in
                    pathsToNode to n (generateTimeTree g 1)

readCSVAndGetTransformedGraph :: String -> Index -> IO (Maybe (WeightedGraph Time, Table (Index, Time)))
readCSVAndGetTransformedGraph filename idx = 
    do
        content <- readFile filename
        let graph = (getGraph filename content) in
            return (executeFunc graph)
        where executeFunc Nothing = Nothing
              executeFunc (Just tg) = Just (getTransformedGraph tg idx)