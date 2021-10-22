{-# language OverloadedStrings #-}

module Page where

import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike)
import Data.Text (isInfixOf, Text)
import Text.URI (mkURI, URI)
import Data.List(find)
import Fetch (correctUrl)

data ImageMeta = ImageMeta {
  imageSrc :: Text,
  imageAltText :: Maybe Text,
  imageTitleText :: Maybe Text
} deriving Show

getImages :: [Tag Text] -> [ImageMeta]
getImages = mapMaybe tagImage
  where
    tagImage (TagOpen "img" attributes) = do
      src <- lookup "src" attributes
      Just ImageMeta { imageSrc =src, imageAltText = lookup "alt" attributes, imageTitleText = lookup "title" attributes}
    tagImage _ = Nothing

data LinkMeta= LinkMeta {
  linkHref :: Text,
  linkRel :: Maybe Text,
  linkInnerText :: Text
} deriving Show

getLinks :: [Tag Text] -> [LinkMeta]
getLinks tags = concatMap tagLinks $ tagTree tags
  where
    tagLinks branch@(TagBranch _ _ children) = maybeToList (tagLink branch) ++ concatMap tagLinks children
      where
        tagLink (TagBranch "a" attributes children) = do
          href <- lookup "href" attributes
          Just LinkMeta { linkHref = href, linkRel = lookup "rel" attributes, linkInnerText = innerText $ flattenTree children }
        tagLink _ = Nothing
    tagLinks _ = []

data ComicPage = ComicPage {
    pageUrl :: URI,
    outputDirectory:: FilePath,
    filePrefix :: String,
    pageNumber :: Int,
    -- | All images matching this predicate will be downloaded
    panelSelect :: ImageMeta -> Bool,
    -- | The first hyperlink matching this predicate will be followed to find the next page
    nextSelect :: LinkMeta -> Bool,
    -- | Skips downloading repeated images *on the same page*
    skipDuplicateImage:: Bool,
    saveTitleText :: Bool,
    ignoreRobotsTxt :: Bool
}

successorPage :: ComicPage -> [LinkMeta] -> Maybe ComicPage
successorPage currentPage links = do
    nextLink <- find (nextSelect currentPage) (filter (isNonCircular . linkToUrl) links)
    nextUrl <- correctUrl (Just $ pageUrl currentPage) (linkHref nextLink)
    pure currentPage {
        pageUrl = nextUrl,
        pageNumber = 1 + pageNumber currentPage
    }
    where
        linkToUrl link = correctUrl (Just $ pageUrl currentPage) (linkHref link)
        isNonCircular Nothing = False
        isNonCircular (Just uri) = uri /= pageUrl currentPage

panelUrlRule :: Text -> ImageMeta -> Bool
panelUrlRule substring image = substring `isInfixOf` imageSrc image

panelAltRule :: Text -> ImageMeta -> Bool
panelAltRule substring image = any (substring `isInfixOf`) $ imageAltText image

linkRelRule :: LinkMeta -> Bool
linkRelRule link = "next" `elem` linkRel link

linkInnerTextRule :: Text -> LinkMeta -> Bool
linkInnerTextRule substring link = substring `isInfixOf` linkInnerText link
