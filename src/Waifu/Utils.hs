<<<<<<< HEAD
module Waifu.Utils where

import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import System.Random (randomRIO)

randomList :: [a] -> IO a
randomList arr = (arr !!) <$> randomRIO (0, length arr - 1)

readDir :: FilePath -> IO [FilePath]
readDir dir = map (dir </>) <$> listDirectory dir
=======
module Waifu.Utils where

import System.Random (randomRIO)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))

randomArray :: [a] -> IO a
randomArray arr = (arr !!) <$> randomRIO (0, length arr)

readDir :: FilePath -> IO [FilePath]
readDir dir = map (dir </>) <$> listDirectory dir 
>>>>>>> 45067e9cd4eaccf955bcb8a740d4c1928105d509
