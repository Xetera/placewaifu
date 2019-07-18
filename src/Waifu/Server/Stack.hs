module Waifu.Server.Stack
    ( WaifuT
    , SetupT
    , runWaifuT
    , runSetupT
    , askRandomImage
    , askSimilarImage
    , loadImages
    ) where

import Control.Monad.Reader

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml as Y

import System.FilePath.Posix
import System.Random (randomRIO)

import Waifu.Image

type WaifuT m = ReaderT [Image] m

type SetupT m = ReaderT FilePath m

runWaifuT :: [Image] -> WaifuT m a -> m a
runWaifuT images f = runReaderT f images

askRandomImage :: MonadIO m => WaifuT m Image
askRandomImage = ask >>= randomList

askSimilarImage :: (Integral a, MonadIO m) => (a, a) -> Float -> WaifuT m Image
askSimilarImage dims ratio = asks (filterSimilarRatio dims ratio) >>= randomList

runSetupT :: FilePath -> SetupT m a -> m a
runSetupT assets f = runReaderT f assets

randomList :: MonadIO m => [a] -> m a
randomList xs = liftIO $ (xs !!) <$> randomRIO (0, length xs - 1)

loadImages :: MonadIO m => SetupT m [Image]
loadImages = do
  ms <- loadMetadata
  traverse (uncurry readImage) $ H.toList ms

loadMetadata :: MonadIO m => SetupT m (H.HashMap String Metadata)
loadMetadata = do
  assets <- ask
  Y.decodeFileThrow $ assets </> "metadata.yaml"

readImage :: MonadIO m => String -> Metadata -> SetupT m Image
readImage name meta = do
  assets <- ask
  img <- liftIO . B.readFile $ assets </> "images" </> name
  pure $ fromByteString (takeBaseName name) meta img
