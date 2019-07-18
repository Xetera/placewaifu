module Waifu.Server.Stack
    ( WaifuM
    , runWaifuM
    , askRandomImage
    , askSimilarImage
    , randomList
    , SetupM
    , runSetupM
    , loadImages
    ) where

import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml as Y

import Servant (Handler)
import System.FilePath.Posix
import System.Random (randomRIO)

import Waifu.Image

newtype WaifuM a = WaifuM (ReaderT [Image] Handler a)
  deriving newtype (Functor, Applicative, Monad, MonadReader [Image], MonadIO)

runWaifuM :: [Image] -> WaifuM a -> Handler a
runWaifuM images (WaifuM f) = runReaderT f images

askRandomImage :: (MonadReader [Image] m, MonadIO m) => m Image
askRandomImage = ask >>= randomList

askSimilarImage :: (MonadReader [Image] m, MonadIO m) => Dimensions -> Double -> m Image
askSimilarImage dims ratio = asks (filterSimilarRatio dims ratio) >>= randomList

randomList :: MonadIO m => [a] -> m a
randomList xs = liftIO $ (xs !!) <$> randomRIO (0, length xs - 1)

newtype SetupM a = SetupM (ReaderT FilePath IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader FilePath, MonadIO)

runSetupM :: FilePath -> SetupM a -> IO a
runSetupM assets (SetupM f) = runReaderT f assets

loadImages :: (MonadReader FilePath m, MonadIO m) => m [Image]
loadImages = do
  ms <- loadMetadata
  traverse (uncurry readImage) $ H.toList ms

loadMetadata :: (MonadReader FilePath m, MonadIO m) => m (H.HashMap String Metadata)
loadMetadata = do
  assets <- ask
  Y.decodeFileThrow $ assets </> "metadata.yaml"

readImage :: (MonadReader FilePath m, MonadIO m) => String -> Metadata -> m Image
readImage name meta = do
  assets <- ask
  img <- liftIO . B.readFile $ assets </> "images" </> name
  pure $ fromByteString (takeBaseName name) meta img
