{-# language OverloadedStrings #-}

module Comic where

import Page
import Data.Text (Text)
import qualified Data.ByteString as BS
import Text.URI (mkURI, URI)
import Data.Maybe (listToMaybe)
import Fetch
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TextEncoding

data ComicPage = ComicPage {
    panelSelect :: ImageMeta -> Bool,
    nextSelect :: LinkMeta -> Bool,
    filePrefix :: String,
    pageUrl :: URI,
    pageNumber :: Int
}

successorPage :: ComicPage -> [LinkMeta] -> Maybe ComicPage
successorPage cur links = do
    nextLink <- listToMaybe $ filter (nextSelect cur) $ filter (\l -> isNonCircular $ linkToUrl l) links
    nextUrl <- correctUrl (Just $ pageUrl cur) (decodeUtf8 $ linkHref nextLink)
    pure ComicPage {
        pageUrl=nextUrl,
        pageNumber= 1 + pageNumber cur,
        panelSelect = panelSelect cur,
        nextSelect = nextSelect cur,
        filePrefix = filePrefix cur
    }
    where
        linkToUrl link = correctUrl (Just $ pageUrl cur) (TextEncoding.decodeUtf8 $ linkHref link)
        isNonCircular Nothing = False
        isNonCircular (Just uri) = uri /= (pageUrl cur)

panelUrlRule :: BS.ByteString -> ImageMeta -> Bool
panelUrlRule substring image = BS.isInfixOf substring $ imageSrc image

linkRelRule :: LinkMeta -> Bool
linkRelRule link = isNext $ linkRel link where
    isNext (Just "next") = True
    isNext _ = False


examplePage :: ComicPage
examplePage = ComicPage {
    pageNumber = 1,
    filePrefix = "xkcd ",
    panelSelect = panelUrlRule "/comics/",
    nextSelect = linkRelRule,
    pageUrl = xkcdStartUri
} where (Just xkcdStartUri) = mkURI "https://xkcd.com/2330/"
