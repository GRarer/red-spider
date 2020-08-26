{-# language OverloadedStrings #-}

module Main where

import Text.URI
import Download
import Comic
import Page
import Fetch
import ParseRules
import qualified Data.Text.Encoding as TextEncoding
import Text.HTML.TagSoup (parseTags)
import Network.HTTP.Req (responseBody)

main :: IO ()
main = visit examplePage


visit :: ComicPage -> IO ()
visit page = do
    res <- get (render $ pageUrl page)
    case res of
        Nothing -> putStrLn "failed to fetch result"
        Just htmlBytes  -> do
            downloadPage page $ filter (panelSelect page) $ getImages tags
            case successorPage page $ getLinks tags of
                Nothing -> putStrLn "reached end of comic"
                Just next -> visit next
            where
                tags = parseTags $ TextEncoding.decodeUtf8 $ responseBody htmlBytes


examplePage :: ComicPage
examplePage = ComicPage {
    pageNumber = 1,
    filePrefix = "xkcd ",
    panelSelect = panelUrlRule "/comics/",
    nextSelect = linkRelRule,
    pageUrl = xkcdStartUri
} where (Just xkcdStartUri) = mkURI "https://xkcd.com/2330/"
