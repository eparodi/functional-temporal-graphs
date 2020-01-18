{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad                      (msum)
import Control.Monad.Trans

import Happstack.Server                   ( Response, ServerPart, defaultBodyPolicy
                                          , decodeBody, dir, lookFile, look, nullConf, ok
                                          , simpleHTTP, toResponse, serveDirectory,
                                          Browsing(EnableBrowsing) )
import Happstack.Server.Internal.RFC822Headers (ContentType)
import Happstack.Server.Internal.Monads

import Html.Core
import Html.Algorithm
import Html.Paths

import CSV

import Definitions
import TemporalGraph.Graph
import TemporalGraph.Algorithms hiding (algorithms)

main :: IO ()
main = simpleHTTP nullConf $ upload

upload :: ServerPart Response
upload =
    do 
        decodeBody (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)
        msum [
            dir "static" $ serveDirectory EnableBrowsing [] "./assets",
            dir "algorithms" $ algorithms,
            dir "paths" $ pathsFormHtml,
            dir "pathsResult" $ paths,
            algorithmFormHtml
            ]

algorithms :: ServerPart Response
algorithms =
    do
        (tmpFile, uploadName, _) <- lookFile "file_upload"
        content <- liftIO $ readFile tmpFile
        fromStr <- look "from"
        toStr <- look "to"
        node_idStr <- look "node_id"
        algorithmStr <- look "alg_type"
        let graph = (getGraph tmpFile content)
            algorithm = getAlgorithm algorithmStr
            node_id = toInt node_idStr
            from = toInt fromStr
            to = toInt toStr in
            findAlgorithmAndReturn algorithm uploadName graph node_id (from, to)
        where findAlgorithmAndReturn :: Maybe TimeAlgorithm -> String -> Maybe TemporalGraph -> Index -> TimeInterval -> ServerPartT IO Response
              findAlgorithmAndReturn Nothing _ _ _ _ = errorResponse "Algorithm not found!"
              findAlgorithmAndReturn _ _ Nothing _ _= errorResponse "Could not parse graph!"
              findAlgorithmAndReturn (Just f) uploadName (Just graph) node_id interval = algorithmHtml uploadName graph (f graph node_id interval)

paths :: ServerPart Response
paths =
    do
        (tmpFile, uploadName, _) <- lookFile "file_upload"
        content <- liftIO $ readFile tmpFile
        fromStr <- look "from"
        toStr <- look "to"
        from_node_idStr <- look "from_node_id"
        to_node_idStr <- look "to_node_id"
        let graph = (getGraph tmpFile content)
            from = toInt fromStr
            to = toInt toStr
            from_node_id = toInt from_node_idStr
            to_node_id = toInt to_node_idStr in
            getGraphAndReturn uploadName graph from to from_node_id to_node_id
        where getGraphAndReturn :: String -> Maybe TemporalGraph -> Time -> Time -> Index -> Index -> ServerPartT IO Response
              getGraphAndReturn _ Nothing _ _ _ _ = errorResponse "Could not parse graph!"
              getGraphAndReturn uploadName (Just graph) from to fromIdx toIdx = 
                pathsView uploadName (between graph (from, to)) fromIdx toIdx 

-- temporalGraph = buildWG (1, 4) [(1, 2, (2, 1)),(1, 2, (14, 1)),(2, 3, (4, 2)),(2, 3, (16, 1)),(3, 4, (8, 2)),(3, 4, (20, 1)),(1, 3, (4, 20)),(4, 1, (12, 1)),(4, 1, (0, 1))]