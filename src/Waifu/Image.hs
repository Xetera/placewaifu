module Waifu.Image where

import Codec.Picture
import Codec.Picture.Metadata
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.Directory
import Waifu.Utils

type ImageM = ExceptT String IO

type ImageData = (DynamicImage, Metadatas)

placeholderPath :: FilePath
placeholderPath = "assets"

getImage :: FilePath -> ImageM ImageData
getImage = ExceptT <$> readImageWithMetadata

allPlaceholders :: ImageM [ImageData]
allPlaceholders = do
  dirs <- liftIO $ readDir placeholderPath
  traverse getImage dirs

randomPlaceholder :: [ImageData] -> IO DynamicImage
randomPlaceholder images = fst <$> randomList images
