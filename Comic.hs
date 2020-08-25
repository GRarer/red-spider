{-# language OverloadedStrings #-}

module Comic where

import Page
import Data.Text (Text)
import qualified Data.ByteString as BS
import Text.URI (mkURI, URI)

data ComicPage = ComicPage {
    panelSelect :: ImageMeta -> Bool,
    nextSelect :: LinkMeta -> Bool,
    filePrefix :: String,
    pageUrl :: URI,
    pageNumber :: Int
}

panelUrlRule :: BS.ByteString -> ImageMeta -> Bool
panelUrlRule substring image = BS.isInfixOf substring $ imageSrc image

linkRelRule :: LinkMeta -> Bool
linkRelRule link = isNext $ linkRel link where
    isNext (Just "rel") = True
    isNext _ = False



examplePage :: ComicPage
examplePage = ComicPage {
    pageNumber = 1,
    filePrefix = "xkcd ",
    panelSelect = panelUrlRule "/comics/",
    nextSelect = linkRelRule,
    pageUrl = xkcdStartUri
} where (Just xkcdStartUri) = mkURI "https://xkcd.com/1/"
