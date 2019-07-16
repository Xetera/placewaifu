module Waifu.Core
    ( Image
    , WaifuT
    , runWaifuT
    ) where

import Control.Monad.Reader

import qualified Graphics.Image as I

type Image = I.Image I.VS I.RGBA Double

type WaifuT m = ReaderT [Image] m

runWaifuT :: MonadIO m => [Image] -> WaifuT m a -> m a
runWaifuT images f = runReaderT f images
