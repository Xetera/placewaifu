module Waifu.Image where

import Waifu.Utils
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Codec.Picture
import Codec.Picture.Metadata
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.Directory
import Waifu.Utils

type ImageM = ExceptT String IO

type ImageData = (DynamicImage, Metadatas)

type Placeholder = ExceptT String IO DynamicImage
type Placeholders = ExceptT String IO [DynamicImage]

placeholderPath :: FilePath
placeholderPath = "assets"

<<<<<<< HEAD
getImage :: FilePath -> ImageM ImageData
getImage = ExceptT <$> readImageWithMetadata

allPlaceholders :: ImageM [ImageData]
=======
getImage :: FilePath -> Placeholder
getImage = ExceptT <$> readImage

allPlaceholders :: Placeholders
>>>>>>> 45067e9cd4eaccf955bcb8a740d4c1928105d509
allPlaceholders = do
  dirs <- liftIO $ readDir placeholderPath
  traverse getImage dirs

<<<<<<< HEAD
randomPlaceholder :: [ImageData] -> IO DynamicImage
randomPlaceholder images = fst <$> randomList images
=======
randomPlaceholder :: Placeholder
randomPlaceholder = allPlaceholders >>= liftIO . randomArray
>>>>>>> 45067e9cd4eaccf955bcb8a740d4c1928105d509
