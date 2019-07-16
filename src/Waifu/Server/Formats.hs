module Waifu.Server.Formats
    ( PNG
    , JPG
    ) where

import qualified Graphics.Image as I

import Servant

import Waifu.Core (Image)

data PNG

instance Accept PNG where
    contentType _ = "image/png"

instance MimeRender PNG Image where
    mimeRender _ = I.encode I.PNG []

data JPG

instance Accept JPG where
    contentType _ = "image/jpg"

instance MimeRender JPG Image where
    mimeRender _ = I.encode I.JPG []
