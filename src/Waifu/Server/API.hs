{-# LANGUAGE TypeApplications #-}

module Waifu.Server.API
  ( api
  ) where

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
server images = hoistServer (Proxy @ImageAPI) (runWaifuM images) serverT

serverT :: ServerT ImageAPI WaifuM
serverT = getRandomResized :<|> getRandomSquare :<|> getRandom :<|> getImages
  where
    getRandomResized :: Word -> Word -> Queries (WaifuM ImageOutput)
    getRandomResized w h = withQueries $ \f -> transform (f . resize (w, h)) <$> askSimilarImage (w, h) 0.3

    getRandomSquare :: Word -> Queries (WaifuM ImageOutput)
    getRandomSquare s = withQueries $ \f -> transform (f . resize (s, s)) <$> askSimilarImage (s, s) 0.4

    getRandom :: Queries (WaifuM ImageOutput)
    getRandom = withQueries $ \f -> transform f <$> askRandomImage

    getImages :: WaifuM [Image]
    getImages = ask
