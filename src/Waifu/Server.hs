{-# LANGUAGE OverloadedStrings #-}

module Waifu.Server where

import Codec.Picture
import Control.Monad.Except
import Control.Monad.Trans.Class
import Network.Wai
import Servant
import Servant.API
import Waifu.Api
import Waifu.Image
  -- withExceptT handleOops $ liftEither image

-- handleOops :: String -> ServantErr
-- handleOops e = err500 {errBody = "oops"}
-- serveImage :: Handler ImageAPI
-- serveImage = do
--   image <- liftIO getImage
server :: Server ImageAPI
server = serveImage

serveImage :: Handler DynamicImage
serveImage = do
  imageE <- liftIO getImage
  case imageE of
    Left _ -> throwError $ err500 {errBody = "made an oopsie"}
    Right image -> return image

app :: Application
app = serve imageApi server
