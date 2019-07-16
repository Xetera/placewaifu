module Waifu.Api where

import Codec.Picture
import Control.Monad.Reader
import Data.Proxy
import Servant.API
import Servant.API.ContentTypes
import Servant.JuicyPixels
import Waifu.Image

type WaifuT m = ReaderT [PlaceholderData] m

type ImageAPI
   = "image" :> Capture "width" Int :> Get '[ PNG] DynamicImage :<|> "image" :> Get '[ PNG] DynamicImage

runWaifuT :: MonadIO m => [PlaceholderData] -> WaifuT m a -> m a
runWaifuT images f = runReaderT f images
