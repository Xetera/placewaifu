{-# LANGUAGE OverloadedStrings #-}

module Waifu.Server where

import Codec.Picture
import Control.Monad.Except
import Control.Monad.Trans.Class
import Network.Wai
import Servant
import Servant.API
import Waifu.Api
import Debug.Trace
import Waifu.Image
import qualified Data.ByteString.Lazy as LBS

server :: Server ImageAPI
server = serveImage

serveImage :: Handler DynamicImage
serveImage = do
  imageE <- liftIO $ runExceptT randomPlaceholder
  case imageE of
    Left e -> do
      liftIO $ putStrLn e
      throwError $ err500 {errBody = "Error occurred"}
    Right image -> return image

app :: Application
app = serve imageApi server
