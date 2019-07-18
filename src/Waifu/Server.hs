module Waifu.Server
  ( runServer
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger

import qualified Data.Text as T

import Network.Wai.Handler.Warp

import Waifu.Server.API
import Waifu.Server.Stack

runServer :: FilePath -> Int -> IO ()
runServer assets port = do
  res <- runSetupM assets loadImages
  runStdoutLoggingT $ case res of
    Right xs -> do
      logInfoN "Starting server..."
      liftIO . run port $ api xs
    Left err -> do
      logErrorN $ "Could not setup server:\n" <> case err of
        OtherSetupException e -> T.pack $ displayException e
        YamlSetupException e  -> T.pack $ displayException e
