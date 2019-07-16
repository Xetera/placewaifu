module Waifu.Resizer where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Metadata as MT
import Codec.Picture.RGBA8
import Data.Maybe
import Waifu.Image

type Width = Int

findClosestWidth :: Width -> [PlaceholderData] -> DynamicImage
findClosestWidth width (x:xs) = image $ foldl (iter width) x xs
  where
    iter :: Width -> PlaceholderData -> PlaceholderData -> PlaceholderData
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

resize :: Width -> PlaceholderData -> DynamicImage
resize width (target, info) =
  case MT.lookup Width info of
    Nothing -> target
    Just size ->
      case MT.lookup Height info of
        Nothing -> target
        Just currentHeight ->
          let ratio = width `div` fromIntegral size
              height = fromIntegral $ currentHeight * fromIntegral ratio
           in ImageRGBA8 $ scaleBilinear width height (fromDynamicImage target)
