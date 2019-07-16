module Waifu.Server.IO
    ( loadImages
    ) where

import qualified Graphics.Image as I

import Waifu.Core
import Waifu.Util

loadImages :: FilePath -> IO (Either String [Image])
loadImages path = do
    dirs <- readDir path
    sequence <$> traverse I.readImage dirs
