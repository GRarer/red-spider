{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Download (downloadPage) where

import Page
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString as BS
import Network.HTTP.Req (responseBody)
import System.Directory ( createDirectoryIfMissing )
import Fetch ( getRelative )
import Text.HTML.TagSoup (parseTags, Tag)
import Text.URI (render, mkURI)
import Data.Maybe (fromJust, listToMaybe)
import Data.Foldable (traverse_)
import System.FilePath ((<.>), (</>))
import Safe (lastMay)
import Codec.ImageType (getImageType)
import Control.Applicative((<|>))

downloadPage :: ComicPage -> [ImageMeta] -> IO ()
downloadPage page [singlePanel] = downloadPanel page Nothing singlePanel
downloadPage page panels = traverse_ download pairs where
    download (image, number) = downloadPanel page (Just number) image
    pairs = zip panels [1..]

downloadPanel :: ComicPage -> Maybe Int -> ImageMeta -> IO ()
downloadPanel page panelNumber imageMeta = do
    possibleResponse <- getRelative (pageUrl page) (imageSrc imageMeta)
    case possibleResponse of
        Nothing -> putStrLn "Error: failed to fetch image"
        Just imageResponse -> do
            createDirectoryIfMissing True outputDirectory
            let contents = responseBody imageResponse
            let fileName = imageFileName imageMeta contents page panelNumber
            BS.writeFile fileName $ responseBody imageResponse

outputDirectory :: FilePath
outputDirectory = "comics-download"

imageFileName :: ImageMeta -> BS.ByteString -> ComicPage -> Maybe Int -> FilePath
imageFileName metadata fileContents page panelNumber = outputDirectory </> name <.> extension
    where
        name = case panelNumber of
            Nothing -> filePrefix page ++ show (pageNumber page)
            Just panelNumber' -> filePrefix page ++ show (pageNumber page) ++ "_" ++ (show panelNumber')
        extensionFromUrl url = do
            end <- lastMay $ Text.splitOn "/" url
            fmap Text.unpack $ lastMay $ Text.splitOn "." url
        extension = fromJust $ getImageType fileContents <|> (extensionFromUrl $ imageSrc metadata) <|> Just "UNKNOWN"
