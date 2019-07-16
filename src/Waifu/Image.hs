module Waifu.Image
  ( Width
  , Placeholder(..)
  , placeholderPath
  , allPlaceholders
  , findClosestWidth
  , resize
  ) where

import Control.Monad.IO.Class

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Metadata as MT
import Codec.Picture.RGBA8

import Waifu.Util

type Width = Int

data Placeholder = Placeholder { image :: DynamicImage, metadata :: Metadatas }

placeholderPath :: FilePath
placeholderPath = "assets"

allPlaceholders :: IO (Either String [Placeholder])
allPlaceholders = do
  dirs <- liftIO $ readDir placeholderPath
  sequence <$> traverse ((fmap . fmap) (uncurry Placeholder) . readImageWithMetadata) dirs

findClosestWidth :: Width -> [Placeholder] -> DynamicImage
findClosestWidth width = image . foldl1 (iter width)
  where
    iter :: Width -> Placeholder -> Placeholder -> Placeholder
    iter target closest current =
      case MT.lookup Width (metadata current) of
        Nothing -> closest
        Just currentWidth ->
          case MT.lookup Width (metadata closest) of
            Nothing -> closest
            Just closestWidth ->
              if abs (fromIntegral currentWidth - target) >
                 abs (fromIntegral closestWidth - target)
                then closest
                else current

resize :: Width -> Placeholder -> DynamicImage
resize width Placeholder { image, metadata } =
  case MT.lookup Width metadata of
    Nothing -> image
    Just size ->
      case MT.lookup Height metadata of
        Nothing -> image
        Just currentHeight ->
          let ratio = width `div` fromIntegral size
              height = fromIntegral $ currentHeight * fromIntegral ratio
           in ImageRGBA8 $ scaleBilinear width height (fromDynamicImage image)
