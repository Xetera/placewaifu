{-# LANGUAGE TypeApplications #-}

module Waifu.Server where

import Codec.Picture
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy as LBS
import Debug.Trace
import Network.Wai
import Servant
import Servant.API
import Waifu.Api
import Waifu.Image
import Waifu.Resizer

server :: [PlaceholderData] -> Server ImageAPI
server placeholders =
  hoistServer (Proxy @ImageAPI) (runWaifuT placeholders) serverT

serverT ::
     forall m. (MonadIO m, MonadError ServantErr m)
  => ServerT ImageAPI (WaifuT m)
serverT = serveWidth :<|> randomImage
  where
    randomImage :: WaifuT m DynamicImage
    randomImage = do
      placeholders <- ask
      placeholder <- liftIO $ randomPlaceholder placeholders
      liftIO . return $ fst placeholder
    serveWidth :: Width -> WaifuT m DynamicImage
    serveWidth width = do
      placeholders <- ask
      -- choice <- liftIO $ randomPlaceholder placeholders
      return $ findClosestWidth width placeholders

app :: [PlaceholderData] -> Application
app = serve (Proxy @ImageAPI) . server
