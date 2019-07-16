module Waifu.Server
  ( app
  ) where

import Control.Monad.Except
import Control.Monad.Reader

import Network.Wai
import Servant

import Waifu.Core
import Waifu.Image
import Waifu.Servant
import Waifu.Util

type ImageAPI
  =    "image" :> Capture "width" Int :> Capture "height" Int :> Get '[PNG, JPG] Image
  :<|> "image" :> Get '[PNG, JPG] Image

app :: [Image] -> Application
app = serve (Proxy @ImageAPI) . server

server :: [Image] -> Server ImageAPI
server images = hoistServer (Proxy @ImageAPI) (runWaifuT images) serverT

serverT :: forall m. (MonadIO m, MonadError ServantErr m) => ServerT ImageAPI (WaifuT m)
serverT = getRandomSized :<|> getRandom
  where
    getRandomSized :: Int -> Int -> WaifuT m Image
    getRandomSized w h = do
      images <- ask
      image <- liftIO $ randomList images
      pure $ resize (w, h) image

    getRandom :: WaifuT m Image
    getRandom = do
      images <- ask
      liftIO $ randomList images
