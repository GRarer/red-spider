{-# language OverloadedStrings #-}

module Main where

import Text.URI (renderStr, render)
import Download
import Comic
import Page
import Fetch
import PageParseRules
import qualified Data.Text.Encoding as TextEncoding
import Text.HTML.TagSoup (parseTags)
import Network.HTTP.Req (responseBody)
import Data.Text (Text)
import Options


main :: IO ()
main = do
    firstPage <- parseOptions
    visit "" firstPage

visit :: Text -> ComicPage -> IO ()
visit previousHTML page = do
    putStrLn $ renderStr $ pageUrl page
    res <- get (render $ pageUrl page)
    case res of
        Nothing -> putStrLn "failed to fetch result"
        Just htmlBytes  -> if html == previousHTML
            then putStrLn "duplicate page found"
            else do
                downloadPage page $ filter (panelSelect page) $ getImages tags
                case successorPage page $ getLinks tags of
                    Nothing -> putStrLn "reached end of comic"
                    Just next -> visit html next
            where
                html = TextEncoding.decodeUtf8 $ responseBody htmlBytes
                tags = parseTags html

