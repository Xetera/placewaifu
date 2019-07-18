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

type Queries a = Bool -> Bool -> a

withQueries :: (ImageTransfrom -> a) -> Queries a
withQueries f x1 x2 = f
  $ (if x1 then greyscale else id)
  . (if x2 then blur else id)

type ImageAPI
  =    "image"
    :> Capture "width" Word
    :> Capture "height" Word
    :> QueryFlag "greyscale"
    :> QueryFlag "blur"
    :> Get '[SVGXML] ImageOutput

  :<|> "image"
    :> Capture "length" Word
    :> QueryFlag "greyscale"
    :> QueryFlag "blur"
    :> Get '[SVGXML] ImageOutput

  :<|> "image"
    :> QueryFlag "greyscale"
    :> QueryFlag "blur"
    :> Get '[SVGXML] ImageOutput

  :<|> "images"
    :> Get '[JSON] [Image]

api :: [Image] -> Application
api = serve (Proxy @ImageAPI) . server

server :: [Image] -> Server ImageAPI
server images = hoistServer (Proxy @ImageAPI) (runWaifuT images) serverT

serverT :: forall m. (MonadIO m, MonadError ServantErr m) => ServerT ImageAPI (WaifuT m)
serverT = getRandomResized :<|> getRandomSquare :<|> getRandom :<|> getImages
  where
    getRandomResized :: Word -> Word -> Queries (WaifuT m ImageOutput)
    getRandomResized w h = withQueries $ \f -> transform (f . resize (w, h)) <$> askSimilarImage (w, h) 0.3

    getRandomSquare :: Word -> Queries (WaifuT m ImageOutput)
    getRandomSquare s = withQueries $ \f -> transform (f . resize (s, s)) <$> askSimilarImage (s, s) 0.4

    getRandom :: Queries (WaifuT m ImageOutput)
    getRandom = withQueries $ \f -> transform f <$> askRandomImage

    getImages :: WaifuT m [Image]
    getImages = ask
