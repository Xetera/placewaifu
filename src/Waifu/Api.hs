module Waifu.Api where

import Codec.Picture
import Control.Monad.Reader
import Data.Proxy
import Servant.API
import Servant.API.ContentTypes
import Servant.JuicyPixels
import Waifu.Image

type WaifuT m = ReaderT [ImageData] m

type ImageAPI = "image" :> Get '[ PNG] DynamicImage

runWaifuT :: MonadIO m => [ImageData] -> WaifuT m a -> m a
runWaifuT images f = runReaderT f images
