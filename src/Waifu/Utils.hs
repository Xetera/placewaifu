module Waifu.Utils where

import System.Random (randomRIO)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))

randomArray :: [a] -> IO a
randomArray arr = (arr !!) <$> randomRIO (0, length arr)

readDir :: FilePath -> IO [FilePath]
readDir dir = map (dir </>) <$> listDirectory dir 