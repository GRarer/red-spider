{-# language OverloadedStrings #-}

module Page where

import Data.Maybe
import Data.Text (Text)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike)

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

data LinkMeta= LinkMeta {linkHref :: Text, linkRel :: Maybe Text, linkInnerText :: Text} deriving Show

getLinks :: [Tag Text] -> [LinkMeta]
getLinks tags = concat $ fmap tagLinks $ tagTree tags where
  tagLinks branch@(TagBranch _ _ children) = (maybeToList $ tagLink branch) ++ (concat $ fmap tagLinks children) where
    tagLink (TagBranch "a" attributes children) = do
      href <- lookup "href" attributes
      Just LinkMeta {linkHref=href, linkRel=lookup "rel" attributes, linkInnerText = innerText $ flattenTree children}
    tagLink _ = Nothing
  tagLinks _ = []
