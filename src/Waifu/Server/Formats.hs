module Waifu.Server.Formats
  ( SVGXML
  ) where

import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as BL

import Servant

import Waifu.Image

data SVGXML

instance Accept SVGXML where
  contentType _ = "image/svg+xml"

instance MimeRender SVGXML Image where
  mimeRender _ = toSVG

toSVG :: Image -> BL.ByteString
toSVG Image {imgBase64, imgSize = (w, h), imgResize = (w', h'), imgFormat} =
  BL.toLazyByteString $
  mconcat
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\""
    , BL.wordDec w'
    , "\" height=\""
    , BL.wordDec h'
    , "\" viewBox=\"0 0 "
    , BL.wordDec w
    , " "
    , BL.wordDec h
    , "\" preserveAspectRatio=\"xMidYMid slice\">"
    , "<image xlink:href=\"data:image/"
    , BL.stringUtf8 imgFormat
    , ";base64,"
    , BL.byteString imgBase64
    , "\" width=\""
    , BL.wordDec w
    , "\" height=\""
    , BL.wordDec h
    , "\"></image>"
    , "</svg>"
    ]
