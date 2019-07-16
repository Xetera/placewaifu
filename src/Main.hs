module Main where

import Network.Wai.Handler.Warp

import Waifu.Image
import Waifu.Server

main :: IO ()
main = do
  mxs <- loadImages "./assets"
  case mxs of
    Left err -> do
      putStrLn "Could not start server"
      putStrLn err
    Right xs -> do
      putStrLn "Starting server..."
      run 1234 (app xs)
