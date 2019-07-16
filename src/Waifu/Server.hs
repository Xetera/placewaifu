module Waifu.Server
  ( app
  ) where

import Control.Monad.Except
import Control.Monad.Reader

import Codec.Picture

import Network.Wai
import Servant
import Servant.JuicyPixels

import Waifu.Image
import Waifu.Util

type WaifuT m = ReaderT [Placeholder] m

runWaifuT :: MonadIO m => [Placeholder] -> WaifuT m a -> m a
runWaifuT images f = runReaderT f images

type ImageAPI
   =    "image" :> Capture "width" Int :> Get '[PNG] DynamicImage
   :<|> "image" :> Get '[PNG] DynamicImage

server :: [Placeholder] -> Server ImageAPI
server placeholders =
  hoistServer (Proxy @ImageAPI) (runWaifuT placeholders) serverT

serverT ::
     forall m. (MonadIO m, MonadError ServantErr m)
  => ServerT ImageAPI (WaifuT m)
serverT = serveWidth :<|> randomImage
  where
    serveWidth :: Width -> WaifuT m DynamicImage
    serveWidth width = do
      placeholders <- ask
      -- choice <- liftIO $ randomPlaceholder placeholders
      return $ findClosestWidth width placeholders

    randomImage :: WaifuT m DynamicImage
    randomImage = do
      placeholders <- ask
      placeholder <- liftIO $ randomList placeholders
      liftIO . return $ image placeholder

app :: [Placeholder] -> Application
app = serve (Proxy @ImageAPI) . server
