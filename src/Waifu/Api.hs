{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Waifu.Api where

import Codec.Picture
import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Servant.API
import Servant.API.ContentTypes
import Servant.JuicyPixels

type ImageAPI = "image" :> Get '[ PNG] DynamicImage

imageApi :: Proxy ImageAPI
imageApi = Proxy
