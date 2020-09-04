{-# language OverloadedStrings #-}

module Page where

import Data.Maybe
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike)
import Data.Text (isInfixOf, Text)
import Text.URI (mkURI, URI)
import Data.Maybe (listToMaybe)
import Fetch (correctUrl)

data ImageMeta = ImageMeta {
  imageSrc :: Text,
  imageAltText :: Maybe Text,
  imageTitleText :: Maybe Text
} deriving Show

getImages :: [Tag Text] -> [ImageMeta]
getImages tags = mapMaybe tagImage tags where
  tagImage (TagOpen "img" attributes) = do
    src <- lookup "src" attributes
    Just ImageMeta { imageSrc=src, imageAltText= lookup "alt" attributes, imageTitleText = lookup "title" attributes}
  tagImage _ = Nothing

data LinkMeta= LinkMeta {
  linkHref :: Text,
  linkRel :: Maybe Text,
  linkInnerText :: Text
} deriving Show

getLinks :: [Tag Text] -> [LinkMeta]
getLinks tags = concat $ fmap tagLinks $ tagTree tags where
  tagLinks branch@(TagBranch _ _ children) = (maybeToList $ tagLink branch) ++ (concat $ fmap tagLinks children) where
    tagLink (TagBranch "a" attributes children) = do
      href <- lookup "href" attributes
      Just LinkMeta {linkHref=href, linkRel=lookup "rel" attributes, linkInnerText = innerText $ flattenTree children}
    tagLink _ = Nothing
  tagLinks _ = []

data ComicPage = ComicPage {
    pageUrl :: URI,
    filePrefix :: String,
    pageNumber :: Int,
    -- all images matching this predicate will be downloaded
    panelSelect :: ImageMeta -> Bool,
    -- the first hyperlink matching this predicate will be followed to find the next page
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

panelUrlRule :: Text -> ImageMeta -> Bool
panelUrlRule substring image = substring `isInfixOf` imageSrc image

panelAltRule :: Text -> ImageMeta -> Bool
panelAltRule substring image = case (imageAltText image) of
    Nothing -> False
    Just alt ->  substring `isInfixOf` alt

linkRelRule :: LinkMeta -> Bool
linkRelRule link = isNext $ linkRel link where
    isNext (Just "next") = True
    isNext _ = False

linkInnerTextRule :: Text -> LinkMeta -> Bool
linkInnerTextRule substring link = substring `isInfixOf` (linkInnerText link)
