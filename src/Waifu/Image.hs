module Waifu.Image where

import Codec.Picture
import Codec.Picture.Metadata
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.Directory
import Waifu.Utils

type Placeholder = DynamicImage

type PlaceholderData = (DynamicImage, Metadatas)

metadata :: PlaceholderData -> Metadatas
metadata = snd

image :: PlaceholderData -> DynamicImage
image = fst

placeholderPath :: FilePath
placeholderPath = "assets"

allPlaceholders :: IO (Either String [PlaceholderData])
allPlaceholders = do
  dirs <- liftIO $ readDir placeholderPath
  sequence <$> traverse readImageWithMetadata dirs

randomPlaceholder :: [PlaceholderData] -> IO PlaceholderData
randomPlaceholder = randomList
