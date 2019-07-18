module Waifu.Server.Servant
    ( SVGXML
    ) where

import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as BL

import Servant

import Waifu.Image

data SVGXML

instance Accept SVGXML where
  contentType _ = "image/svg+xml"

instance MimeRender SVGXML ImageOutput where
  mimeRender _ = uncurry toSVG

toSVG :: Image -> ImageOptions -> BL.ByteString
toSVG
  Image { imgBase64, imgSize = (w, h), imgFormat }
  opts@ImageOptions { optSize = (w', h') }
  = BL.toLazyByteString $ mconcat
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\""
    , BL.wordDec w'
    , "\" height=\""
    , BL.wordDec h'
    , "\" viewBox=\"0 0 "
    , BL.wordDec w
    , " "
    , BL.wordDec h
    , "\" preserveAspectRatio=\"xMidYMid slice\">"
    , if null filters then "" else "<filter id=\"filter\">" <> mconcat filters <> "</filter>"
    , "<image xlink:href=\"data:image/"
    , BL.stringUtf8 imgFormat
    , ";base64,"
    , BL.byteString imgBase64
    , "\" width=\""
    , BL.wordDec w
    , "\" height=\""
    , BL.wordDec h
    , "\" "
    , if null filters then "" else "filter=\"url(#filter)\""
    , "></image>"
    , "</svg>"
    ]
  where filters = makeFilters opts

makeFilters :: ImageOptions -> [BL.Builder]
makeFilters ImageOptions { optGreyscale, optBlur } = mconcat
  [ when optGreyscale  $ "<feColorMatrix type=\"saturate\" values=\"0.0\"></feColorMatrix>"
  , when optBlur       $ "<feGaussianBlur type=\"blur\" stdDeviation=\"5\"></feGaussianBlur>"
  ]
  where when p x = if p then [x] else []
