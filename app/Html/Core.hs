{-# LANGUAGE OverloadedStrings #-}

module Html.Core where

import Happstack.Server.Internal.RFC822Headers (ContentType)
import Happstack.Server                   (Response, ok, toResponse)

import Text.Blaze                         as B
import Text.Blaze.Internal                as B
import Text.Blaze.Html5                   as B hiding (map)
import Text.Blaze.Html5.Attributes        as B hiding (dir, title, form, label)

import TemporalGraph.Graph
import WeightedGraph.Graph
import Definitions

import Happstack.Server.Internal.Monads
import Control.Monad (forM_)

getHead :: String -> Html
getHead titleStr =
    B.head $ do
        title (toHtml titleStr)
        link !
            rel "stylesheet" !
            href "static/bootstrap.min.css"
        link !
            rel "stylesheet" !
            href "static/main.css"

errorResponse :: String -> ServerPartT IO Response
errorResponse str = ok $ toResponse $
    baseTemplate "" (
        B.div $ do
            h5 "Error"
            p $ do toHtml str
    )

getActiveLink :: String -> String -> String
getActiveLink str1 str2 
    | str1 == str2 = "active"
    | otherwise = ""

baseTemplate :: String -> Html -> Html
baseTemplate activeLink html_ = 
    html $ do
        getHead "Upload Form"
        body ! class_ "body-text-center" $ do
            B.div ! class_ "cover-container d-flex w-100 h-100 p-3 mx-auto flex-column" $ do
                header ! class_ "masthead mb-auto" $ do
                    B.div ! class_ "inner" $ do
                        h3 ! class_ "masthead-brand" $ do 
                            B.a ! href "/" $ do "Graphkell"
                    nav ! class_ "nav nav-masthead justify-content-center" $ do
                        B.a ! href "/" ! class_ (toValue ("nav-link " ++ (getActiveLink "algorithms" activeLink))) $ do "Algorithms"
                        B.a ! href "/paths" ! class_ (toValue ("nav-link " ++ (getActiveLink "paths" activeLink))) $ do "Paths"
                main ! role "main" ! class_ "inner cover" $ do
                    html_
                footer ! class_ "mastfoot mt-auto" $ do
                    B.div ! class_ "inner" $ do
                        p "ITBA - Eliseo Parodi Almaraz 2020"

temporalGraphInfoTemplate :: String -> TemporalGraph -> Html
temporalGraphInfoTemplate fileName g =
    let gList = toList g
    in 
        B.div ! class_ "card dark-border" $ do
            B.div ! class_ "card-body dark" $ do
                h5 (toHtml fileName)
                ul ! class_ "flex-evenly" $ do
                    li (toHtml $ (show (nodes g)) ++ " nodes")
                    li (toHtml $ (show (edges g)) ++ " edges")
