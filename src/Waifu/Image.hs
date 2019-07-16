module Waifu.Image
  ( Placeholder(..)
  , height
  , width
  , loadPlaceholders
  , findClosest
  , resize
  ) where

import Control.Monad.IO.Class

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Metadata as MT
import Codec.Picture.RGBA8

import Waifu.Util

data Placeholder = Placeholder
  { image :: DynamicImage
  , metadata :: Metadatas
  }

height :: Placeholder -> Maybe Word
height = MT.lookup Height . metadata

width :: Placeholder -> Maybe Word
width = MT.lookup Width . metadata

loadPlaceholders :: FilePath -> IO (Either String [Placeholder])
loadPlaceholders path = do
  dirs <- liftIO $ readDir path
  xs <- sequence <$> traverse readImageWithMetadata dirs
  pure $ (fmap . fmap) (uncurry Placeholder) xs

findClosest :: Word -> [Placeholder] -> DynamicImage
findClosest targetWidth = image . foldl1 closer
  where
    dist x y = abs (x - y)
    closer x y = maybe x id $ do
      wx <- fromIntegral <$> width x
      wy <- fromIntegral <$> width y
      if dist wx targetWidth <= dist wy targetWidth
        then pure x
        else pure y

resize :: Word -> Placeholder -> DynamicImage
resize targetWidth placeholder = maybe (image placeholder) id $ do
  h <- height placeholder
  w <- width placeholder
  let ratio = targetWidth `div` fromIntegral w
      w' = fromIntegral w
      h' = fromIntegral $ h * fromIntegral ratio
  pure . ImageRGBA8 . scaleBilinear w' h' . fromDynamicImage $ image placeholder
