{-# LANGUAGE OverloadedStrings #-}

module Html.Paths where

import Html.Core

import Happstack.Server.Internal.RFC822Headers (ContentType)
import Happstack.Server                   (Response, ok, toResponse)

import Text.Blaze                         as B
import Text.Blaze.Internal                as B
import Text.Blaze.Html5                   as B hiding (map)
import Text.Blaze.Html5.Attributes        as B hiding (dir, title, form, label)

import TemporalGraph.Graph
import TemporalGraph.PathAlgorithms
import WeightedGraph.Graph
import Definitions

import Happstack.Server.Internal.Monads
import Control.Monad (forM_)

pathsFormHtml :: ServerPart Response
pathsFormHtml = ok $ toResponse $
    baseTemplate "paths" (
        B.div $ do
            h5 "Analize graph uploading a CSV"
            form ! enctype "multipart/form-data" ! B.method "POST" ! action "/pathsResult" $ do
                B.div ! class_ "form-group row" $ do
                    B.label ! for "file_upload" ! class_ "col-sm-2 col-form-label" $ do "CSV File"
                    B.div ! class_ "col-sm-10" $ do
                        input ! type_ "file" ! name "file_upload" ! size "40" ! B.id "file_upload" ! class_ "btn btn-secondary form-control-file" ! required "true"
                B.div ! class_ "row" $ do
                    B.div ! class_ "form-group col-sm-3 col-6" $ do
                        B.label ! for "node_id" ! class_ "col-form-label" $ do "From Node ID"
                        B.div $ do
                            input ! type_ "number" ! name "from_node_id" ! B.id "from_node_id" ! class_ "form-control" ! required "true"
                    B.div ! class_ "form-group col-sm-3 col-6" $ do
                        B.label ! for "node_id" ! class_ "col-form-label" $ do "To Node ID"
                        B.div $ do
                            input ! type_ "number" ! name "to_node_id" ! B.id "to_node_id" ! class_ "form-control" ! required "true"
                    B.div ! class_ "form-group col-sm-3 col-6" $ do
                        B.label ! for "from" ! class_ "col-form-label" $ do "After/From"
                        B.div $ do
                            input ! type_ "number" ! name "from" ! B.id "from" ! class_ "form-control" ! required "true"
                    B.div ! class_ "form-group col-sm-3 col-6" $ do
                        B.label ! for "to" ! class_ "col-form-label" $ do "Before/To"
                        B.div $ do
                            input ! type_ "number" ! name "to" ! B.id "to" ! class_ "form-control" ! required "true"
                -- B.div ! class_ "alg-type-buttons" $ do
                --     B.div ! class_ "form-check col-sm-6" $ do
                --         input ! type_ "radio" ! class_ "form-check-input" ! B.id "alg_type1" ! name "alg_type" ! value "shortestPath" ! required "true"
                --         B.label ! for "alg_type1" ! class_ "form-check-label" $ do "Shortest Path"
                --     B.div ! class_ "form-check col-sm-6" $ do
                --         input ! type_ "radio" ! class_ "form-check-input" ! B.id "alg_type2" ! name "alg_type" ! value "earliestArrivalPath" ! required "true"
                --         B.label ! for "alg_type2" ! class_ "form-check-label" $ do "Earliest Arrival Path Path"
                --     B.div ! class_ "form-check col-sm-6" $ do
                --         input ! type_ "radio" ! class_ "form-check-input" ! B.id "alg_type3" ! name "alg_type" ! value "latestDeparturePath" ! required "true"
                --         B.label ! for "alg_type3" ! class_ "form-check-label" $ do "Latest Departure Path"
                --     B.div ! class_ "form-check col-sm-6" $ do
                --         input ! type_ "radio" ! class_ "form-check-input" ! B.id "alg_type4" ! name "alg_type" ! value "fastestPath" ! required "true"
                        -- B.label ! for "alg_type4" ! class_ "form-check-label" $ do "Fastest Path"
                input ! class_ "btn btn-primary" ! type_ "submit" ! value "Upload!"
    )

pathsResultTemplate :: String -> ([Index], Maybe TimeInterval) -> Html
pathsResultTemplate title (_, Nothing) =
    B.div ! class_ "card dark-border" $ do
        B.div ! class_ "card-body dark" $ do
            h5 (toHtml title)
            p "No path."
pathsResultTemplate title (idxs, Just (t1, t2)) =
    B.div ! class_ "card dark-border" $ do
        B.div ! class_ "card-body dark" $ do
            h5 (toHtml title)
            p (toHtml (show idxs))
            p (toHtml $ do (show t1) ++ " - " ++ (show t2))

pathsView :: String -> TemporalGraph -> Index -> Index -> ServerPartT IO Response
pathsView fileName g from to = 
    ok $ toResponse $ do
        baseTemplate "paths" ( do
            temporalGraphInfoTemplate fileName g
            pathsResultTemplate "Shortest Path"         (shortestPath g from to)
            pathsResultTemplate "Fastest Path"          (fastestPath g from to)
            pathsResultTemplate "Earliest Arrival Path" (earliestArrivalPath g from to)
            pathsResultTemplate "Latest Departure Path" (shortestPath g from to))
