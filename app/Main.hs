module Main where

import Waifu.Server

main :: IO ()
main = runServer "./assets/images" 1234
