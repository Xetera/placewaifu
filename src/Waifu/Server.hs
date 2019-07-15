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

server :: Server ImageAPI
server = do
  placeholderE <- liftIO $ runExceptT allPlaceholders
  case placeholderE of
    Left e -> throwError $ err500 {errBody = "Error occurred"}
    Right placeholders ->
      hoistServer (Proxy @ImageAPI) (runWaifuT placeholders) serveImage

serveImage :: (MonadIO m, MonadError ServantErr m) => WaifuT m DynamicImage
serveImage = do
  images <- ask
  liftIO $ randomPlaceholder images

app :: Application
app = serve (Proxy @ImageAPI) server
-- runServer :: ImageM ()
-- runServer = do
--   placeholders <- allPlaceholders
--   run 1234 app
