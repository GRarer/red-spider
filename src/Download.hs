{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Download (downloadPage) where

import Page
import Fetch ( getRelative )

import Control.Applicative((<|>))
import Data.Semigroup (Last(..))

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString as BS
import Network.HTTP.Req (responseBody)
import System.Directory ( createDirectoryIfMissing )
import Text.HTML.TagSoup (parseTags, Tag)
import Text.URI (render, mkURI)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Foldable (traverse_)
import System.FilePath ((<.>), (</>))
import Codec.ImageType (getImageType)
import Control.Monad (when)

downloadPage :: ComicPage -> [ImageMeta] -> IO ()
downloadPage page [singlePanel] = downloadPanel page Nothing singlePanel
downloadPage page panels = traverse_ downloadNumberedPanel numberedPanels
    where
        numberedPanels = zip panels [1..]
        downloadNumberedPanel (image, number) = downloadPanel page (Just number) image

downloadPanel :: ComicPage -> Maybe Int -> ImageMeta -> IO ()
downloadPanel page panelNumber imageMeta = do
    possibleResponse <- getRelative (pageUrl page) (imageSrc imageMeta)
    case possibleResponse of
        Nothing -> putStrLn "Error: failed to fetch image"
        Just imageResponse -> do
            createDirectoryIfMissing True (outputDirectory page)
            let contents = responseBody imageResponse
            let imagePath = getImageFilePath page panelNumber imageMeta contents
            BS.writeFile imagePath $ responseBody imageResponse
            when (saveTitleText page) $ do
                let path = getFileName page panelNumber <.> "txt"
                traverse_ (Text.writeFile path) $ imageTitleText imageMeta

lastMaybe :: Foldable f => f a -> Maybe a
lastMaybe = fmap getLast . foldMap (Just . Last)

getImageFilePath :: ComicPage -> Maybe Int -> ImageMeta -> BS.ByteString -> FilePath
getImageFilePath page panelNumber imageMetadata imageContents =
    let
        -- getting the extension from the end of the URL is a fallback for when getImageType can't identify the format
        extensionFromUrl = fmap Text.unpack $ lastMaybe $ Text.splitOn "/" $ imageSrc imageMetadata
        extension = fromMaybe "UNKNOWN" $ getImageType imageContents <|> extensionFromUrl
    in getFileName page panelNumber <.> extension

getFileName :: ComicPage -> Maybe Int -> FilePath
getFileName page panelNumber = outputDirectory page </> name
    where
        name = case panelNumber of
            Nothing -> filePrefix page ++ show (pageNumber page)
            Just panelNumber' -> filePrefix page ++ show (pageNumber page) ++ "_" ++ show panelNumber'
