{-# LANGUAGE TypeApplications #-}

module Waifu.Server
  ( app
  ) where

import Control.Monad.Except
import Control.Monad.Reader

import Network.Wai
import Servant

import Servant.HTML.Blaze
import Text.Blaze.Html as H
import Waifu.Core
import Waifu.Image
import Waifu.Servant
import Waifu.Server.Views.Home
import Waifu.Util

type ImageAPI
   = "image" :> Capture "height" Int :> Capture "width" Int :> Get '[ PNG, JPG] Image :<|> "image" :> Get '[ PNG, JPG] Image :<|> Get '[ HTML] H.Html

app :: [Image] -> Application
app = serve (Proxy @ImageAPI) . server

server :: [Image] -> Server ImageAPI
server images = hoistServer (Proxy @ImageAPI) (runWaifuT images) serverT

serverT ::
     forall m. (MonadIO m, MonadError ServantErr m)
  => ServerT ImageAPI (WaifuT m)
serverT = getRandomSized :<|> getRandom :<|> homePage
  where
    homePage :: WaifuT m H.Html
    homePage = return home
    getRandomSized :: Int -> Int -> WaifuT m Image
    getRandomSized h w = do
      images <- ask
      image <- liftIO $ randomList images
      pure $ resize (h, w) image
    getRandom :: WaifuT m Image
    getRandom = do
      images <- ask
      liftIO $ randomList images
