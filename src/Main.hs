{-# language OverloadedStrings #-}

module Main where

import Text.URI (renderStr, URI)
import Download
import Page
import Fetch (get)
import Robots (createRobotRule)
import qualified Data.Text.Encoding as TextEncoding
import Text.HTML.TagSoup (parseTags)
import Network.HTTP.Req (responseBody)
import Data.Text (Text)
import Options (parseOptions)
import Data.Text.Encoding.Error (lenientDecode)


main :: IO ()
main = do
    firstPage <- parseOptions
    let firstPageUrl = pageUrl firstPage
    urlAllowed <- createRobotRule firstPageUrl (ignoreRobotsTxt  firstPage)
    if urlAllowed firstPageUrl
        then visit firstPage "" urlAllowed
        else putStrLn "The specified URL is disallowed by robots.txt"

visit :: ComicPage -> Text -> (URI -> Bool) -> IO ()
visit page previousHTML urlAllowed = do
    putStrLn $ renderStr $ pageUrl page
    res <- get $ pageUrl page
    case res of
        Nothing -> putStrLn "failed to fetch page"
        Just htmlBytes ->
            let
                html = TextEncoding.decodeUtf8With lenientDecode $ responseBody htmlBytes
                tags = parseTags html
            in if html == previousHTML
                then putStrLn "duplicate page found"
                else do
                    downloadPage page $ filter (panelSelect page) $ getImages tags
                    case successorPage page $ getLinks tags of
                        Nothing -> putStrLn "reached end of comic"
                        Just next -> if urlAllowed $ pageUrl next
                            then visit next html urlAllowed
                            else putStrLn "The next page is disallowed by robots.txt"

