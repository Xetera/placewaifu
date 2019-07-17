module Main where

import Options.Applicative
import Waifu.Server

data Options = Options
    { assetsFolder :: String
    }

options :: Parser Options
options = Options
    <$> option str (long "assets" <> short 'a' <> help "location of assets folder")

argsParse :: ParserInfo Options
argsParse = info (helper <*> options)
    (fullDesc <> progDesc "Run placewaifu" <> header "An incredibly cute placeholder service")

main :: IO ()
main = do
    Options { assetsFolder } <- execParser argsParse
    runServer assetsFolder 1234
