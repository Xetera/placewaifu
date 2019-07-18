module Waifu.Server.Servant
    ( QueryFlags
    , SVGXML
    ) where

import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import qualified Data.Text as T

import GHC.TypeLits

import Network.HTTP.Types (parseQueryText)
import Network.Wai (Request, rawQueryString)
import Servant
import Servant.Server.Internal (passToServer)

import Waifu.Image

data QueryFlags (xs :: [Symbol])

class ListSymbols (xs :: [Symbol]) where
  listSymbols :: Proxy xs -> [String]

instance ListSymbols '[] where
  listSymbols _ = []

instance (KnownSymbol x, ListSymbols xs) => ListSymbols (x ': xs) where
  listSymbols _ = symbolVal (Proxy @x) : listSymbols (Proxy @xs)

instance (HasServer api context, ListSymbols xs) => HasServer (QueryFlags xs :> api) context where
  type ServerT (QueryFlags xs :> api) m = S.Set String -> ServerT api m

  route _ context server = route (Proxy @api) context (passToServer server $ S.fromList . params (Proxy @xs))
    where
      params :: Proxy xs -> Request -> [String]
      params _ r = filter (param r) $ listSymbols (Proxy @xs)

      queryText r = parseQueryText $ rawQueryString r
      param r p = maybe False (const True) $ lookup (T.pack p) (queryText r)

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s

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
