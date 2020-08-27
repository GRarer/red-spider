{-# language OverloadedStrings #-}

module PageParseRules where

import Data.Text (Text, isInfixOf)
import Page

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
