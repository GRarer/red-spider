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
import Control.Monad (when, void, unless, foldM_)
import Data.ByteString (ByteString)
import Page (ImageMeta(ImageMeta))
import Data.Set (Set, size, insert, empty, member)


downloadPage :: ComicPage -> [ImageMeta] -> IO ()
downloadPage page [singlePanel] = void $ downloadPanel page empty Nothing singlePanel
downloadPage page panels = foldM_ downloadNumberedPanel empty panels
    where
        downloadNumberedPanel :: Set ByteString -> ImageMeta -> IO(Set ByteString)
        downloadNumberedPanel visitedPanels image = do
            let panelNumber = size visitedPanels + 1
            panelContents <- downloadPanel page visitedPanels (Just panelNumber) image
            case panelContents of
                Just contents -> return (insert contents visitedPanels)
                Nothing -> return visitedPanels

downloadPanel :: ComicPage -> Set ByteString -> Maybe Int -> ImageMeta -> IO (Maybe ByteString)
downloadPanel page pagePanels panelNumber imageMeta = do
    possibleResponse <- getRelative (pageUrl page) (imageSrc imageMeta)
    case possibleResponse of
        Nothing -> do
            putStrLn "Error: failed to fetch image"
            return Nothing
        Just imageResponse -> do
            createDirectoryIfMissing True (outputDirectory page)
            let contents = responseBody imageResponse
            let imagePath = getImageFilePath page panelNumber imageMeta contents
            unless (skipDuplicateImage page && member contents pagePanels) $ do
                BS.writeFile imagePath contents
                when (saveTitleText page) $ do
                    let path = getFileName page panelNumber <.> "txt"
                    traverse_ (Text.writeFile path) $ imageTitleText imageMeta
            return $ Just contents

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
