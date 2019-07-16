module Main where

import Waifu.Server

main :: IO ()
main = runServer "./assets" 1234
