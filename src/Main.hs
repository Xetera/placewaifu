module Main where

import Network.Wai.Handler.Warp

import Waifu.Image
import Waifu.Server

main :: IO ()
main = do
  placeholdersE <- allPlaceholders
  case placeholdersE of
    Left err -> do
      putStrLn "Could not start server"
      putStrLn err
    Right placeholders -> do
      putStrLn "Starting server..."
      run 1234 (app placeholders)
