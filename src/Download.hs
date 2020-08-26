{-# language OverloadedStrings #-}

module Download where

import Page

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString as BS
import Network.HTTP.Req (responseBody)
import System.Directory
import Util (split)
import Comic
import Fetch
import Text.HTML.TagSoup (parseTags, Tag)
import Text.URI (render)
import Data.Maybe (listToMaybe)

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

downloadPage :: ComicPage -> [ImageMeta] -> IO ()
downloadPage page (singlePanel:[]) = downloadPanel page Nothing singlePanel
downloadPage page panels = mapM_ download pairs where
    download = \(image, number) -> downloadPanel page (Just number) image
    pairs = zip panels [1..]

downloadPanel :: ComicPage -> Maybe Int -> ImageMeta -> IO ()
downloadPanel page panelNumber imageMeta = do
    putStrLn $ "attempting to download image " ++ (show $ imageSrc imageMeta) ++ "to file: "
    putStrLn fileName
    possibleResponse <- getRelative (pageUrl page) (imageSrc imageMeta)
    case possibleResponse of
        Nothing -> putStrLn "Error: failed to fetch image"
        Just imageResponse -> do
            createDirectoryIfMissing True outputDirectory
            BS.writeFile fileName $ responseBody imageResponse
            putStrLn $ "saved " ++ (show $ imageSrc imageMeta) ++ " to " ++ fileName
    where fileName = imageFileName imageMeta page panelNumber

outputDirectory :: String
outputDirectory = "./comics-download/"

imageFileName :: ImageMeta -> ComicPage -> Maybe Int -> FilePath
imageFileName meta page panelNumber = "./comics-download/" ++ name ++ '.':extension where
    extension = last $ split '.' $ Text.unpack $ imageSrc meta
    prefix = (filePrefix page) ++ (show $ pageNumber page)
    name = case panelNumber of
        Nothing -> prefix
        Just panNum -> prefix ++ ' ':(show panNum)
