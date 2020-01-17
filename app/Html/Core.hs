{-# LANGUAGE OverloadedStrings #-}
module Html.Core where

import Happstack.Server.Internal.RFC822Headers (ContentType)
import Happstack.Server                   (Response, ok, toResponse)

import Text.Blaze                         as B
import Text.Blaze.Internal                as B
import Text.Blaze.Html5                   as B hiding (map)
import Text.Blaze.Html5.Attributes        as B hiding (dir, title, form, label)

import TemporalGraph.Graph
import Happstack.Server.Internal.Monads

getHead :: String -> Html
getHead titleStr =
    B.head $ do
        title (toHtml titleStr)
        link !
            rel "stylesheet" !
            href "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
        link !
            rel "stylesheet" !
            href "static/main.css"

postHtml :: (FilePath, FilePath, ContentType) -> Maybe TemporalGraph -> ServerPartT IO Response
postHtml r Nothing = 
    ok $ toResponse $
        baseTemplate (
            p "Error! CSV file is corrupted"
        )
postHtml r (Just g) = 
    ok $ toResponse $
        baseTemplate (
            postBody r (show g)
        )

postBody :: (FilePath, FilePath, ContentType) -> String -> Html
postBody (tmpFile, uploadName, contentType) g = do
    p (toHtml $ "temporary file: " ++ g)
    p (toHtml $ "uploaded name:  " ++ uploadName)
    p (toHtml $ "content-type:   " ++ show contentType)

postFormHtml :: ServerPart Response
postFormHtml = ok $ toResponse $
    baseTemplate (
        B.div $ do
            h5 "Analize graph uploading a CSV"
            form ! enctype "multipart/form-data" ! B.method "POST" ! action "/post" $ do
                B.div ! class_ "form-group" $ do
                    B.label ! for "file_upload" $ do "CSV File"
                    input ! type_ "file" ! name "file_upload" ! size "40" ! B.id "file_upload" ! class_ "btn btn-secondary form-control-file"
                input ! class_ "btn btn-primary" ! type_ "submit" ! value "Upload!"
            
    )

baseTemplate :: Html -> Html
baseTemplate html_ = 
    html $ do
        getHead "Upload Form"
        body ! class_ "text-center" $ do
            B.div ! class_ "cover-container d-flex w-100 h-100 p-3 mx-auto flex-column" $ do
                header ! class_ "masthead mb-auto" $ do
                    B.div ! class_ "inner" $ do
                        h3 ! class_ "masthead-brand" $ do 
                            B.a ! href "/" $ do"Graphkell"
                main ! role "main" ! class_ "inner cover" $ do
                    html_
                footer ! class_ "mastfoot mt-auto" $ do
                    B.div ! class_ "inner" $ do
                        p "ITBA - Eliseo Parodi Almaraz 2020"
        