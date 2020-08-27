module Options where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (asum)
import Text.URI (mkURI, URI)
import Data.Text (pack)

data PanelSelectMode = PanelModeSrc String | PanelModeAlt String deriving Show
data LinkSelectMode = LinkModeRel | LinkModeInnerText String deriving Show

data Options = Options {
    firstUrl :: URI,
    prefix :: String,
    initialNumber :: Int,
    panelSelectMode :: PanelSelectMode,
    linkSelectMode :: LinkSelectMode
} deriving Show

uriReader :: ReadM URI
uriReader = eitherReader $ (\input -> case (mkURI $ pack input) of
    Nothing -> Left "Invalid URL"
    Just uri' -> Right uri')

parser :: Parser Options
parser = Options
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
            (PanelModeSrc <$> strOption (
                long "panelSrc"
                <> metavar "SUBSTRING"
                <> help "Select panel images that have a specific substring in their `src` url"
                )),
            (PanelModeAlt <$> strOption (
                long "panelAlt"
                <> metavar "SUBSTRING"
                <> help "Select panel images that have a specific substring in their `alt` attribute"
                )),
            pure $ PanelModeSrc "/comics/"
        ]
    <*> asum
        [
            (LinkModeInnerText <$> strOption (
                long "linkText"
                <> metavar "SUBSTRING"
                <> help ("Locate link to next page by looking for a substring within the link's inner text" ++
                    " (overrides default behavior of identifying links based on their `rel` attribute)")
                )),
            pure $ LinkModeRel
        ]

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "do some stuff with some params"
     <> header "hello - a test for optparse-applicative" )


