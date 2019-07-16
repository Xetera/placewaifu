module Waifu.Image
  ( loadImages
  , findClosest
  , resize
  ) where

import qualified Graphics.Image as I

import Waifu.Core
import Waifu.Util

loadImages :: FilePath -> IO (Either String [Image])
loadImages path = do
  dirs <- readDir path
  sequence <$> traverse I.readImage dirs

findClosest :: Int -> [Image] -> Image
findClosest targetWidth = foldl1 closer
  where
    dist x y = abs (x - y)
    closer x y =
      let wx = I.cols x
          wy = I.cols y
      in if dist wx targetWidth <= dist wy targetWidth
          then x
          else y

resize :: (Int, Int) -> Image -> Image
resize = I.resize I.Bilinear I.Edge
