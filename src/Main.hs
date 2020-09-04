{-# language OverloadedStrings #-}

module Main where

import Text.URI (renderStr, render)
import Download
import Page
import Fetch ( get )
import qualified Data.Text.Encoding as TextEncoding
import Text.HTML.TagSoup (parseTags)
import Network.HTTP.Req (responseBody)
import Data.Text (Text)
import Options ( parseOptions )


main :: IO ()
main = do
    firstPage <- parseOptions
    visit firstPage ""

visit :: ComicPage -> Text -> IO ()
visit page previousHTML = do
    putStrLn $ renderStr $ pageUrl page
    res <- get $ pageUrl page
    case res of
        Nothing -> putStrLn $ "failed to fetch page"
        Just htmlBytes  -> if html == previousHTML
            then putStrLn "duplicate page found"
            else do
                downloadPage page $ filter (panelSelect page) $ getImages tags
                case successorPage page $ getLinks tags of
                    Nothing -> putStrLn "reached end of comic"
                    Just next -> visit next html
            where
                html = TextEncoding.decodeUtf8 $ responseBody htmlBytes
                tags = parseTags html
