{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Waifu.Api where

import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Servant.API

type ImageAPI = "image" :> Get '[ JSON] [String]

imageApi :: Proxy ImageAPI
imageApi = Proxy
