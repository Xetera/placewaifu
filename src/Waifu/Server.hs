module Waifu.Server
  ( runServer
  ) where

import Network.Wai.Handler.Warp

import Waifu.Server.API
import Waifu.Server.IO

runServer :: FilePath -> Int -> IO ()
runServer assets port = do
  mxs <- loadImages assets
  case mxs of
    Left err -> do
      putStrLn "Could not start server"
      putStrLn err
    Right xs -> do
      putStrLn "Starting server..."
      run port $ api xs
