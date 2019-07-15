module Lib
  ( start
  ) where

import Network.Wai.Handler.Warp
import Waifu.Server

start :: IO ()
start = run 1234 app
