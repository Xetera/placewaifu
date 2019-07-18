{-# LANGUAGE TypeApplications #-}

module Waifu.Server.API
  ( api
  ) where

import Control.Monad.Reader

import qualified Data.Set as S

import Servant
import qualified Data.Text as T
import Waifu.Image
import Waifu.Server.Servant
import Waifu.Server.Stack

type ImageHeaders = Headers '[Header "Cache-Control" T.Text]
type ImageResponse = ImageHeaders ImageOutput

imageHeaders :: Word -> a -> ImageHeaders a
imageHeaders duration = addHeader $ "public, max-age=" <> (T.pack $ show duration)

defaultImageHeaders :: a -> ImageHeaders a
defaultImageHeaders = imageHeaders 86400

type TransformQueries = S.Set String

fromQueries :: TransformQueries -> ImageTransform
fromQueries = foldl (.) id . map toTransform . S.toList
  where
    toTransform = \case
      "greyscale" -> greyscale
      "blur"      -> blur
      _           -> error "unsupported transformation"

type ImageAPI
  =    "image"
    :> Capture "width" Word
    :> Capture "height" Word
    :> QueryFlags '["greyscale", "blur"]
    :> Get '[SVGXML] ImageResponse

  :<|> "image"
    :> Capture "length" Word
    :> QueryFlags '["greyscale", "blur"]
    :> Get '[SVGXML] ImageResponse

  :<|> "image"
    :> QueryFlags '["greyscale", "blur"]
    :> Get '[SVGXML] ImageResponse

  :<|> "images"
    :> Get '[JSON] [Image]

api :: [Image] -> Application
api = serve (Proxy @ImageAPI) . server

server :: [Image] -> Server ImageAPI
server images = hoistServer (Proxy @ImageAPI) (runWaifuM images) serverT

serverT :: ServerT ImageAPI WaifuM
serverT = getRandomResized :<|> getRandomSquare :<|> getRandom :<|> getImages
  where
    getRandomResized :: Word -> Word -> TransformQueries -> WaifuM ImageResponse
    getRandomResized w h qs
      = defaultImageHeaders . transform (fromQueries qs . resize (w, h)) <$> askSimilarImage (w, h) 0.3

    getRandomSquare :: Word -> TransformQueries -> WaifuM ImageResponse
    getRandomSquare s qs
      = defaultImageHeaders . transform (fromQueries qs . resize (s, s)) <$> askSimilarImage (s, s) 0.4

    getRandom :: TransformQueries -> WaifuM ImageResponse
    getRandom qs
      = defaultImageHeaders . transform (fromQueries qs) <$> askRandomImage

    getImages :: WaifuM [Image]
    getImages = ask
