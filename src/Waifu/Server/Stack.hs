module Waifu.Server.Stack
    ( WaifuT
    , runWaifuT
    , askRandomImage
    , loadImages
    ) where

import Control.Monad.Reader

import qualified Data.ByteString as B

import Waifu.Image
import Waifu.Util

type WaifuT m = ReaderT [Image] m

runWaifuT :: MonadIO m => [Image] -> WaifuT m a -> m a
runWaifuT images f = runReaderT f images

askRandomImage :: MonadIO m => WaifuT m Image
askRandomImage = ask >>= liftIO . randomList

loadImages :: FilePath -> IO [Image]
loadImages path = do
  dirs <- readDir path
  xs <- traverse B.readFile dirs
  pure $ map fromByteString xs
