module Waifu.Server
  ( runServer
  ) where

import Network.Wai.Handler.Warp

import Waifu.Server.API
import Waifu.Server.Stack

runServer :: FilePath -> Int -> IO ()
runServer assetsFolder port = do
  xs <- loadImages assetsFolder
  putStrLn "Starting server..."
  run port $ api xs
