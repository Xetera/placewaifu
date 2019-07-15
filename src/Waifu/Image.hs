module Waifu.Image where

import Codec.Picture
import System.Directory

placeholderPath :: FilePath
placeholderPath = "assets"

getImage :: IO (Either String DynamicImage)
getImage = readImage "assets/kawaii.jpg"

allPlaceholders :: IO (Either String [DynamicImage])
allPlaceholders = do
  dirs <- listDirectory placeholderPath
  sequence <$> traverse readImage dirs
