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
<<<<<<< HEAD
main = runServer "./assets/images" 1234
=======
main = do
    Options { assetsFolder } <- execParser argsParse
    runServer assetsFolder 1234
>>>>>>> ee12698eaec5f7e93f4200c1d7d935a66cf12c3a
