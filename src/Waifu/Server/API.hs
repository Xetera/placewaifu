{-# LANGUAGE TypeApplications #-}

module Waifu.Server.API
  ( api
  ) where

import Control.Monad.Except
import Control.Monad.Reader

import Servant

import Servant.HTML.Blaze
import Servant.Server.StaticFiles
import Text.Blaze.Html5 as H
import Waifu.Core
import Waifu.Image
import Waifu.Server.Formats
import Waifu.Server.Views.Home
import Waifu.Util

type ImageAPI
   = "image" :> Capture "width" Int :> Capture "height" Int :> Get '[ PNG, JPG] Image :<|> "image" :> Get '[ PNG, JPG] Image :<|> Get '[ HTML] H.Html :<|> "assets" :> Raw

api :: [Image] -> Application
api = serve (Proxy @ImageAPI) . server

server :: [Image] -> Server ImageAPI
server images = hoistServer (Proxy @ImageAPI) (runWaifuT images) serverT

serverT ::
     forall m. (MonadIO m, MonadError ServantErr m)
  => ServerT ImageAPI (WaifuT m)
serverT = getRandomSized :<|> getRandom :<|> homePage :<|> static
  where
    static = serveDirectoryFileServer "./assets"
    homePage :: WaifuT m H.Html
    homePage = return home
    getRandomSized :: Int -> Int -> WaifuT m Image
    getRandomSized w h = do
      images <- ask
      image <- liftIO $ randomList images
      pure $ resize (w, h) image
    getRandom :: WaifuT m Image
    getRandom = do
      images <- ask
      liftIO $ randomList images
