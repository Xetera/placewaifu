module Waifu.Image where

import Codec.Picture

getImage :: IO (Either String DynamicImage)
getImage = readImage "assets/cute.png"
