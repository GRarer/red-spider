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
import Text.HTML.TagSoup (Tag)
import Text.URI (render)

visit :: ComicPage -> IO ()
visit page = do
    res <- fetchTags page
    case res of
        Nothing -> putStrLn "failed to fetch result"
        Just tags  -> downloadPanel page firstImage where
            firstImage = head $ filter rule allImages where
                allImages = getImages tags
                rule = panelSelect page



fetchTags :: ComicPage -> IO (Maybe [Tag BS.ByteString])
fetchTags page = do
    res <- get (render $ pageUrl page)
    case res of
        Nothing -> pure Nothing
        Just bytesResponse  -> pure $ Just $ getTags bytesResponse



downloadPanel :: ComicPage -> ImageMeta -> IO ()
downloadPanel page meta = do
    putStrLn $ "attempting to download image " ++ (show $ imageSrc meta)
    possibleResponse <- getRelative (pageUrl page) (TextEncoding.decodeUtf8 $ imageSrc meta)
    case possibleResponse of
        Nothing -> putStrLn "Error: failed to fetch image"
        Just imageResponse -> do
            createDirectoryIfMissing True outputDirectory
            BS.writeFile "example.jpg" $ responseBody imageResponse

outputDirectory :: FilePath
outputDirectory = "/comics-download/"

imageFileName :: ImageMeta -> Int -> FilePath
imageFileName meta pageNumber = outputDirectory ++ show pageNumber ++ extension where
    extension = last $ split '.' $ show $ imageSrc meta
