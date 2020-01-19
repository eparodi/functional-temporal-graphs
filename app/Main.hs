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
        decodeBody (defaultBodyPolicy "/tmp/" (2^30) 1000 (2^30))
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
        where findAlgorithmAndReturn :: Maybe (TimeAlgorithm, String) -> String -> Maybe TemporalGraph -> Index -> TimeInterval -> ServerPartT IO Response
              findAlgorithmAndReturn Nothing _ _ _ _ = errorResponse "Algorithm not found!"
              findAlgorithmAndReturn _ _ Nothing _ _= errorResponse "Could not parse graph!"
              findAlgorithmAndReturn (Just (f, name)) uploadName (Just graph) node_id interval = 
                algorithmHtml uploadName graph (f graph node_id interval) name

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

-- temporalGraph = buildWG (1, 50) [(1,5,(1448651209,10717)),(1,11,(1375254229,32035)),(2,8,(1557163903,5763)),(2,20,(1260120513,30864)),(3,16,(1310309302,12855)),(3,15,(1565518423,21791)),(3,17,(1151372628,35139)),(3,2,(1577684929,12535)),(4,16,(1402012875,16663)),(4,8,(1258065520,28376)),(5,1,(1366304855,8918)),(5,14,(1281840806,24912)),(5,12,(1514484954,32549)),(5,18,(1077388126,16163)),(6,19,(1576115085,42107)),(6,15,(1356520958,24436)),(6,16,(1330556159,34193)),(6,5,(1525401753,24401)),(7,11,(1020829980,8031)),(7,2,(1456547279,39507)),(7,6,(1485960345,41966)),(8,10,(1276610664,20680)),(8,4,(1465920279,20482)),(9,6,(1303123398,10037)),(9,15,(1455690783,29407)),(10,9,(1501727034,21697)),(10,12,(972417106,5709)),(10,13,(1425534500,20854)),(11,3,(1556269074,35104)),(11,6,(1549100381,10461)),(11,4,(1030023602,12546)),(11,5,(1553902959,16176)),(12,16,(1172901868,32684)),(12,7,(1219016074,13516)),(12,17,(1366935877,41960)),(12,14,(1155788755,25419)),(13,5,(1222295134,7454)),(13,7,(1198195981,12804)),(13,7,(1036744366,7722)),(13,18,(1229484868,22240)),(14,17,(1126416435,32497)),(14,16,(1086485574,29362)),(15,19,(1213160492,26025)),(15,9,(1438516743,37867)),(16,6,(1065935030,11612)),(16,7,(1101492817,28102)),(17,8,(994394319,16118)),(17,3,(1014949528,23378)),(18,7,(1257587311,8158)),(18,16,(1045228436,12652)),(18,20,(1199502555,33352)),(18,13,(1003847565,32427)),(19,8,(1206138264,30471)),(19,13,(1181468422,24902)),(20,14,(1341681837,22878)),(20,16,(1433874375,21372)),(20,10,(1521907222,23619))]