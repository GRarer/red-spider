{-# language OverloadedStrings #-}

module Options (parseOptions) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (asum)
import Text.URI (isPathAbsolute, mkScheme, makeAbsolute, mkURI, URI)
import Data.Text (pack)
import Page
import Fetch(correctUrl)
import Data.Maybe (fromJust)
import Data.List (isInfixOf)
import Control.Monad (guard)

urlReader :: ReadM URI
urlReader = eitherReader $ \input ->
    let
        httpsScheme = fromJust $ mkScheme "https"
        parseAbsoluteUrl urlString = do
            uri <- mkURI $ pack urlString
            let uriAbsolute = makeAbsolute httpsScheme uri
            guard (isPathAbsolute uriAbsolute)
            pure uriAbsolute
        uri = parseAbsoluteUrl input <|> parseAbsoluteUrl ("//" ++ input)
    in maybe (Left "Invalid URL") Right uri

parser :: Parser ComicPage
parser = ComicPage
    <$> option urlReader (mconcat [
            long "url",
            metavar "URL",
            help "URL of first page"
        ])
    <*> strOption (mconcat [
            long "prefix",
            short 'p',
            metavar "PREFIX",
            help "Prefix label for output file names",
            value ""
        ])
    <*> option auto (mconcat [
            long "number",
            short 'n',
            help "Number to label the first page",
            showDefault,
            value 1,
            metavar "N"
        ])
    <*> asum [
            panelUrlRule <$> strOption (mconcat [
                long "panelSrc",
                metavar "SUBSTRING",
                help "Select panel images that have a specific substring in their `src` url"
            ]),
            panelAltRule <$> strOption (mconcat [
                long "panelAlt",
                metavar "SUBSTRING",
                help "Select panel images that have a specific substring in their `alt` attribute"
            ]),
            pure (panelUrlRule "/comics/")
        ]
    <*> asum [
            linkInnerTextRule <$> strOption (mconcat [
                long "linkText",
                metavar "SUBSTRING",
                help "Locate link to next page by looking for a substring within the link's inner text (overrides default behavior of identifying links based on their `rel` attribute)"
            ]),
            pure linkRelRule
        ]
    <*> switch (mconcat [
            long "saveTitleText",
            short 't',
            help "Save title-text for each panel as text files"
        ])

parseOptions :: IO ComicPage
parseOptions = execParser opts
  where
    opts = info (parser <**> helper)
        (mconcat [
            fullDesc,
            header "red-spider, a tool for downloading webcomics",
            progDesc "Red Spider reads through a webcomic and downloads images from each page"
        ])


