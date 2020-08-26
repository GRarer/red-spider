{-# language OverloadedStrings #-}

module ExampleComics where

import Text.URI
import Comic
import ParseRules

xkcd :: ComicPage
xkcd = ComicPage {
    pageNumber = 1,
    filePrefix = "xkcd ",
    panelSelect = panelUrlRule "/comics/",
    nextSelect = linkRelRule,
    pageUrl = xkcdStartUri
} where (Just xkcdStartUri) = mkURI "https://xkcd.com/1/"


oHumanStar :: ComicPage
oHumanStar = ComicPage {
    pageNumber = 1,
    filePrefix = "ohs ",
    panelSelect = panelAltRule "Chapter",
    nextSelect = linkInnerTextRule "Next",
    pageUrl = xkcdStartUri
} where (Just xkcdStartUri) = mkURI "http://ohumanstar.com/comic/chapter-1-title-page/"
