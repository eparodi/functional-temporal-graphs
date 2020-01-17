{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad                      (msum)
import Control.Monad.Trans

import Happstack.Server                   ( Response, ServerPart, defaultBodyPolicy
                                          , decodeBody, dir, lookFile, nullConf, ok
                                          , simpleHTTP, toResponse, serveDirectory,
                                          Browsing(EnableBrowsing) )
import Happstack.Server.Internal.RFC822Headers (ContentType)
import Happstack.Server.Internal.Monads

import Html.Core

import CSV
import TemporalGraph.Graph

main :: IO ()
main = simpleHTTP nullConf $ upload

upload :: ServerPart Response
upload =
    do 
        decodeBody (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)
        msum [
            dir "static" $ serveDirectory EnableBrowsing [] "./assets",
            dir "post" $ post,
            postFormHtml
            ]

post :: ServerPart Response
post =
    do
        (tmpFile, uploadName, contentType) <- lookFile "file_upload"
        content <- liftIO $ readFile tmpFile
        let graph = (getGraph tmpFile content) in
            postHtml (tmpFile, uploadName, contentType) graph
