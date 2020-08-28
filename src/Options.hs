{-# language OverloadedStrings #-}

module Options where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (asum)
import Text.URI (mkURI, URI)
import Data.Text (pack)
import Comic
import PageParseRules


uriReader :: ReadM URI
uriReader = eitherReader $ (\input -> case (mkURI $ pack input) of
    Nothing -> Left "Invalid URL"
    Just uri' -> Right uri')

parser :: Parser ComicPage
parser = ComicPage
    <$> option uriReader
    ( long "url"
    <> metavar "URL"
    <> help "URL of first page")
    <*> strOption
        ( long "prefix"
        <> short 'p'
        <> metavar "PREFIX"
        <> help "Prefix label for output file names"
        <> value "")
    <*> option auto
        ( long "number"
        <> short 'n'
        <> help "Number to label the first page"
        <> showDefault
        <> value 1
        <> metavar "N" )
    <*> asum
        [
            (panelUrlRule <$> strOption (
                long "panelSrc"
                <> metavar "SUBSTRING"
                <> help "Select panel images that have a specific substring in their `src` url"
                )),
            (panelAltRule <$> strOption (
                long "panelAlt"
                <> metavar "SUBSTRING"
                <> help "Select panel images that have a specific substring in their `alt` attribute"
                )),
            pure $ panelUrlRule "/comics/"
        ]
    <*> asum
        [
            (linkInnerTextRule <$> strOption (
                long "linkText"
                <> metavar "SUBSTRING"
                <> help ("Locate link to next page by looking for a substring within the link's inner text" ++
                    " (overrides default behavior of identifying links based on their `rel` attribute)")
                )),
            pure $ linkRelRule
        ]

parseOptions :: IO ComicPage
parseOptions = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "do some stuff with some params"
     <> header "hello - a test for optparse-applicative" )


