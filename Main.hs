{-# language OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Req
import Text.URI

import qualified Data.ByteString.Char8 as BSC
import Page

main :: IO ()
main = do
  res <- get "https://en.wikipedia.org/wiki/Firewatch#Plot"
  case res of
    Nothing -> putStrLn "Error: malformed URL"
    Just x  -> BSC.putStrLn $ responseBody x
