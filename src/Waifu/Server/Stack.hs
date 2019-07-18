module Waifu.Server.Stack
    ( WaifuM
    , runWaifuM
    , askRandomImage
    , askSimilarImage
    , randomList
    , SetupM
    , SetupException(..)
    , runSetupM
    , loadImages
    ) where

import Control.Exception hiding (Handler)
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml as Y

import Servant (Handler)
import System.FilePath.Posix
import System.Random (randomRIO)

import Waifu.Image

newtype WaifuM a = WaifuM { unWaifuM :: ReaderT [Image] (LoggingT Handler) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader [Image], MonadLogger, MonadIO)

runWaifuM :: [Image] -> WaifuM a -> Handler a
runWaifuM images = runStdoutLoggingT . flip runReaderT images . unWaifuM

askRandomImage :: (MonadReader [Image] m, MonadIO m) => m Image
askRandomImage = ask >>= randomList

askSimilarImage :: (MonadReader [Image] m, MonadIO m) => Dimensions -> Double -> m Image
askSimilarImage size allowedError = asks (filterSimilarRatio size allowedError) >>= randomList

randomList :: MonadIO m => [a] -> m a
randomList xs = liftIO $ (xs !!) <$> randomRIO (0, length xs - 1)

newtype SetupM a = SetupM { unSetupM :: ReaderT FilePath (ExceptT SetupException IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader FilePath, MonadError SetupException, MonadIO)

data SetupException
  = YamlSetupException Y.ParseException
  | OtherSetupException SomeException

runSetupM :: FilePath -> SetupM a -> IO (Either SetupException a)
runSetupM assets = runExceptT . flip runReaderT assets . unSetupM

loadImages :: (MonadReader FilePath m, MonadError SetupException m, MonadIO m) => m [Image]
loadImages = do
  ms <- loadMetadata
  traverse (uncurry readImage) $ H.toList ms

loadMetadata :: (MonadReader FilePath m, MonadError SetupException m, MonadIO m) => m (H.HashMap String Metadata)
loadMetadata = do
  assets <- ask
  res <- liftIO . Y.decodeFileEither $ assets </> "metadata.yaml"
  case res of
    Right x  -> pure x
    Left err -> throwError $ YamlSetupException err

readImage :: (MonadReader FilePath m, MonadError SetupException m, MonadIO m) => String -> Metadata -> m Image
readImage name meta = do
  assets <- ask
  res <- liftIO . try $ do
    img <- B.readFile $ assets </> "images" </> name
    pure $ fromByteString (takeBaseName name) meta img
  case res of
    Right x  -> pure x
    Left err -> throwError $ OtherSetupException err
