{-# language OverloadedStrings #-}

module Page where

import Data.Proxy

import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Text as Text
import Network.HTTP.Req
import Text.URI
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike)


xkcd :: Text.Text
xkcd = "https://xkcd.com/1/"

get :: Text.Text -> IO (Maybe BsResponse)
get url = runReq httpCfg $ do
  case useURI =<< (mkURI url) of
    Nothing  -> pure Nothing
    Just uri -> Just <$> either getBS getBS uri
  where
    getBS (uri, schema) = req GET uri NoReqBody (Proxy :: Proxy BsResponse) schema
    httpCfg = defaultHttpConfig

getTags :: BsResponse -> [Tag ByteString]
getTags response = parseTags $ responseBody response

data ImageMeta = ImageMeta {imageSrc :: ByteString, imageAltText :: Maybe ByteString} deriving Show

getImages :: [Tag ByteString] -> [ImageMeta]
getImages tags = mapMaybe tagImage tags where
  tagImage (TagOpen "img" attributes) = do
    src <- lookup "src" attributes
    Just ImageMeta { imageSrc=src, imageAltText= lookup "alt" attributes}
  tagImage _ = Nothing

data LinkMeta= LinkMeta {linkHref :: ByteString, linkRel :: Maybe ByteString, linkInnerText :: ByteString} deriving Show

getLinks :: [Tag ByteString] -> [LinkMeta]
getLinks tags = concat $ fmap tagLinks $ tagTree tags where
  tagLinks branch@(TagBranch _ _ children) = (maybeToList $ tagLink branch) ++ (concat $ fmap tagLinks children) where
    tagLink (TagBranch "a" attributes children) = do
      href <- lookup "href" attributes
      Just LinkMeta {linkHref=href, linkRel=lookup "rel" attributes, linkInnerText = innerText $ flattenTree children}
    tagLink _ = Nothing
  tagLinks _ = []
