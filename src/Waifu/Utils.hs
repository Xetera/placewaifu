module Waifu.Utils where

import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import System.Random (randomRIO)

randomList :: [a] -> IO a
randomList arr = (arr !!) <$> randomRIO (0, length arr - 1)

readDir :: FilePath -> IO [FilePath]
readDir dir = map (dir </>) <$> listDirectory dir
