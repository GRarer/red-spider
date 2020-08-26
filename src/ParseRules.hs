{-# language OverloadedStrings #-}

module ParseRules where

import Data.Text (Text, isInfixOf)
import Page

panelUrlRule :: Text -> ImageMeta -> Bool
panelUrlRule substring image = isInfixOf substring $ imageSrc image

linkRelRule :: LinkMeta -> Bool
linkRelRule link = isNext $ linkRel link where
    isNext (Just "next") = True
    isNext _ = False
