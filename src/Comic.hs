{-# language OverloadedStrings #-}

module Comic where

import Page
import Data.Text (isInfixOf, Text)
import Text.URI (mkURI, URI)
import Data.Maybe (listToMaybe)
import Fetch (correctUrl)

data ComicPage = ComicPage {
    pageUrl :: URI,
    filePrefix :: String,
    pageNumber :: Int,
    panelSelect :: ImageMeta -> Bool,
    nextSelect :: LinkMeta -> Bool

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
