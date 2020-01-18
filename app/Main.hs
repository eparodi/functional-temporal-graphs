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
            algorithmFormHtml
            ]

algorithms :: ServerPart Response
algorithms =
    do
        (tmpFile, uploadName, contentType) <- lookFile "file_upload"
        content <- liftIO $ readFile tmpFile
        from <- look "from"
        to <- look "to"
        node_id <- look "node_id"
        algorithm <- look "alg_type"
        let graph = (getGraph tmpFile content) in
            findAlgorithmAndReturn (getAlgorithm algorithm) uploadName graph (toInt node_id) ((toInt from), (toInt to))
        where findAlgorithmAndReturn :: Maybe TimeAlgorithm -> String -> Maybe TemporalGraph -> Index -> TimeInterval -> ServerPartT IO Response
              findAlgorithmAndReturn Nothing _ _ _ _ = errorResponse "Algorithm not found!"
              findAlgorithmAndReturn _ _ Nothing _ _= errorResponse "Could not parse graph!"
              findAlgorithmAndReturn (Just f) uploadName (Just graph) node_id interval = algorithmHtml uploadName graph (f graph node_id interval)
