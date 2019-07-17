{-# LANGUAGE TypeApplications #-}

module Waifu.Server.API
  ( api
  ) where

import Control.Monad.Except
import Control.Monad.Reader

import Servant

import Waifu.Image
import Waifu.Server.Servant
import Waifu.Server.Stack

type ImageAPI
<<<<<<< HEAD
   = "image" :> Capture "width" Word :> Capture "height" Word :> Get '[ SVGXML] Image
   :<|> "image" :> Capture "length" Word :> Get '[ SVGXML] Image
   :<|> "image" :> Get '[ SVGXML] Image
=======
  =    "image" :> Capture "width" Word :> Capture "height" Word :> Get '[SVGXML] Image
  :<|> "image" :> Capture "length" Word :> Get '[SVGXML] Image
  :<|> "image" :> Get '[SVGXML] Image
  :<|> "images" :> Get '[JSON] [Image]
>>>>>>> ee12698eaec5f7e93f4200c1d7d935a66cf12c3a

api :: [Image] -> Application
api = serve (Proxy @ImageAPI) . server

server :: [Image] -> Server ImageAPI
server images = hoistServer (Proxy @ImageAPI) (runWaifuT images) serverT

<<<<<<< HEAD
serverT ::
     forall m. (MonadIO m, MonadError ServantErr m)
  => ServerT ImageAPI (WaifuT m)
serverT = getRandomResized :<|> getRandomSquare :<|> getRandom
=======
serverT :: forall m. (MonadIO m, MonadError ServantErr m) => ServerT ImageAPI (WaifuT m)
serverT = getRandomResized :<|> getRandomSquare :<|> getRandom :<|> getImages
>>>>>>> ee12698eaec5f7e93f4200c1d7d935a66cf12c3a
  where
    getRandomResized :: Word -> Word -> WaifuT m Image
    getRandomResized w h = resize (w, h) <$> askRandomImage
    getRandomSquare :: Word -> WaifuT m Image
    getRandomSquare s = resize (s, s) <$> askRandomImage
    getRandom :: WaifuT m Image
    getRandom = askRandomImage

    getImages :: WaifuT m [Image]
    getImages = ask
