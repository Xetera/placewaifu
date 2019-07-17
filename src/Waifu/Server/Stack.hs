module Waifu.Server.Stack
    ( WaifuT
    , runWaifuT
    , askRandomImage
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

runWaifuT :: MonadIO m => [Image] -> WaifuT m a -> m a
runWaifuT images f = runReaderT f images

askRandomImage :: MonadIO m => WaifuT m Image
askRandomImage = ask >>= liftIO . randomList

randomList :: [a] -> IO a
randomList xs = (xs !!) <$> randomRIO (0, length xs - 1)

loadMetadata :: FilePath -> IO (H.HashMap String Metadata)
loadMetadata assetsFolder = Y.decodeFileThrow $ assetsFolder </> "metadata.yaml"

loadImages :: FilePath -> IO [Image]
loadImages assetsFolder = do
  ms <- loadMetadata assetsFolder
  traverse (uncurry $ readImage assetsFolder) $ H.toList ms

readImage :: FilePath -> String -> Metadata -> IO Image
readImage assetsFolder name meta = do
  img <- B.readFile $ assetsFolder </> "images" </> name
  pure $ fromByteString (takeBaseName name) meta img
