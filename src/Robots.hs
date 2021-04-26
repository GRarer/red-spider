{-# LANGUAGE LambdaCase #-}
{-# language OverloadedStrings #-}

module Robots (createRobotRule) where

import Fetch (getRelative)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import Network.HTTP.Req ( responseBody, HttpException (VanillaHttpException), BsResponse)
import Text.URI
import Data.Text (Text)
import Network.HTTP.Robots (Robot, parseRobots, canAccess)
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (try)
import Control.Monad (join)

-- fetches robots.txt and returns a function that evaluates whether crawling a given URL is allowed
createRobotRule :: URI -> Bool -> IO (URI -> Bool)
createRobotRule startUrl ignoreRobotsTxt = do
  robot <- if ignoreRobotsTxt then pure Nothing else fetchRobot startUrl
  pure $ evaluateRobot robot

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = \case
  Right x -> Just x
  Left _  -> Nothing

fetchRobot :: URI -> IO (Maybe Robot)
fetchRobot url = do
  result <- try $ getRelative url "/robots.txt" :: IO (Either HttpException (Maybe BsResponse))
  let contents = join $ eitherToMaybe result
  case contents of
    Nothing -> pure Nothing
    Just response -> pure $ eitherToMaybe $ parseRobots $ responseBody response

-- checks whether crawling a URI is allowed by the site's robots.txt
evaluateRobot :: Maybe Robot -> URI -> Bool
evaluateRobot r uri = case r of
  Nothing -> True
  Just robot -> canAccess "" robot (encodeUtf8 $ renderPath uri)

-- renders the path part of a URI
-- we always include both leading and trailing commas to match the formatting of robots.txt files
renderPath :: URI -> Text
renderPath URI{uriPath = Nothing} = ""
renderPath URI{uriPath = Just (_, pathParts)} = "/" <> base <> "/"
  where base = Text.intercalate "/" (fmap Text.URI.unRText (NonEmpty.toList pathParts))
