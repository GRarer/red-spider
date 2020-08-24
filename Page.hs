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

isImgTag :: Tag ByteString -> Bool
isImgTag (TagOpen "img" attributes) = any isSrcAttr attributes where
  isSrcAttr ("src", _) = True
  isSrcAttr _ = False
isImgTag _ = False

data ImageMeta = ImageMeta {imageSrc :: ByteString, imageAltText :: Maybe ByteString} deriving Show

tagImage :: Tag ByteString -> Maybe ImageMeta
tagImage (TagOpen "img" attributes) = do
  src <- lookup "src" attributes
  Just ImageMeta { imageSrc=src, imageAltText= lookup "alt" attributes}
tagImage _ = Nothing

getImages :: [Tag ByteString] -> [ImageMeta]
getImages tags = mapMaybe tagImage tags


