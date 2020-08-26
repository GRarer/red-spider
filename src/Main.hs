{-# language OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Req
import Text.URI

import qualified Data.ByteString.Char8 as BSC
import Page
import Download
import Comic

main :: IO ()
main = visit examplePage
