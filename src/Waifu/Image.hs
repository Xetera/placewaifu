module Waifu.Image where

import Waifu.Utils
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Codec.Picture
import System.Directory

type Placeholder = ExceptT String IO DynamicImage
type Placeholders = ExceptT String IO [DynamicImage]

placeholderPath :: FilePath
placeholderPath = "assets"

getImage :: FilePath -> Placeholder
getImage = ExceptT <$> readImage

allPlaceholders :: Placeholders
allPlaceholders = do
  dirs <- liftIO $ readDir placeholderPath
  traverse getImage dirs

randomPlaceholder :: Placeholder
randomPlaceholder = allPlaceholders >>= liftIO . randomArray