module Waifu.Util
  ( randomList
  , readDir
  ) where

import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import System.Random (randomRIO)

randomList :: [a] -> IO a
randomList xs = (xs !!) <$> randomRIO (0, length xs - 1)

readDir :: FilePath -> IO [FilePath]
readDir dir = map (dir </>) <$> listDirectory dir
