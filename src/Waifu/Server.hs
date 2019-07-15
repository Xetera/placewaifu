{-# LANGUAGE OverloadedStrings #-}

module Waifu.Server where

import Network.Wai
import Servant
import Servant.API
import Waifu.Api

server :: Server ImageAPI
server = return ["hello"]

app :: Application
app = serve imageApi server
