{-# language OverloadedStrings #-}

module Fetch where

import Data.Proxy
import qualified Data.Text as Text
import Network.HTTP.Req
import Text.URI
import Control.Monad (guard)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text ( Text )

get :: URI -> IO (Maybe BsResponse)
get url = runReq httpCfg $ do
  case (useURI url) of
    Nothing  -> pure Nothing
    Just uri -> Just <$> either getBS getBS uri
  where
    getBS (uri, schema) = req GET uri NoReqBody (Proxy :: Proxy BsResponse) schema
    httpCfg = defaultHttpConfig


getRelative :: URI -> Text -> IO (Maybe BsResponse)
getRelative sourcePage url = case correctedUri of
  Nothing -> pure Nothing
  Just uri -> get uri
  where correctedUri = correctUrl (Just sourcePage) url


correctUrl :: Maybe URI -> Text.Text -> Maybe URI
correctUrl sourcePage url = do
    uri <- mkURI url
    -- try to use source page as a base if the url is relative, and default to
    -- https if scheme is not present
    let uri' = makeAbsolute httpsScheme $ fromMaybe uri (relativeTo uri =<< sourcePage)
    -- if it's still not absolute, there's nothing we can do to figure it out
    guard (isPathAbsolute uri')
    pure uri'
  where
    httpsScheme = fromJust $ mkScheme "https"

