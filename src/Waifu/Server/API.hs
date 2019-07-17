module Waifu.Server.API
    ( api
    ) where

import Control.Monad.Except

import Servant

import Waifu.Image
import Waifu.Server.Servant
import Waifu.Server.Stack

type ImageAPI
  =    "image" :> Capture "width" Word :> Capture "height" Word :> Get '[SVGXML] Image
  :<|> "image" :> Capture "length" Word :> Get '[SVGXML] Image
  :<|> "image" :> Get '[SVGXML] Image

api :: [Image] -> Application
api = serve (Proxy @ImageAPI) . server

server :: [Image] -> Server ImageAPI
server images = hoistServer (Proxy @ImageAPI) (runWaifuT images) serverT

serverT :: forall m. (MonadIO m, MonadError ServantErr m) => ServerT ImageAPI (WaifuT m)
serverT = getRandomResized :<|> getRandomSquare :<|> getRandom
  where
    getRandomResized :: Word -> Word -> WaifuT m Image
    getRandomResized w h = resize (w, h) <$> askRandomImage

    getRandomSquare :: Word -> WaifuT m Image
    getRandomSquare s = resize (s, s) <$> askRandomImage

    getRandom :: WaifuT m Image
    getRandom = askRandomImage
