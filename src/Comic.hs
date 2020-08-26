{-# language OverloadedStrings #-}

module Comic where

import Page
import Data.Text (isInfixOf, Text)
import Text.URI (mkURI, URI)
import Data.Maybe (listToMaybe)
import Fetch (correctUrl)

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
    nextUrl <- correctUrl (Just $ pageUrl cur) (linkHref nextLink)
    pure ComicPage {
        pageUrl=nextUrl,
        pageNumber= 1 + pageNumber cur,
        panelSelect = panelSelect cur,
        nextSelect = nextSelect cur,
        filePrefix = filePrefix cur
    }
    where
        linkToUrl link = correctUrl (Just $ pageUrl cur) (linkHref link)
        isNonCircular Nothing = False
        isNonCircular (Just uri) = uri /= (pageUrl cur)

panelUrlRule :: Text -> ImageMeta -> Bool
panelUrlRule substring image = isInfixOf substring $ imageSrc image

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
