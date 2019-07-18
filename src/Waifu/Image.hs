module Waifu.Image
    ( Image(..)
    , Metadata(..)
    , fromByteString
    , ImageOptions(..)
    , ImageOutput
    , ImageTransfrom
    , transform
    , resize
    , greyscale
    , blur
    , baseOptions
    , filterSimilarRatio
    ) where

import Data.List
import Data.Ord
import qualified Codec.Picture as P
import qualified Codec.Picture.Metadata as P
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B
import qualified Data.Text.Encoding as T

data Image = Image
  { imgName   :: !String
  , imgMeta   :: !Metadata
  , imgBase64 :: !B.ByteString
  , imgSize   :: !(Word, Word)
  , imgFormat :: !String
  }

instance ToJSON Image where
  toJSON Image { imgName, imgMeta, imgBase64, imgSize, imgFormat } = object
    [ "name"   .= imgName
    , "width"  .= fst imgSize
    , "height" .= snd imgSize
    , "format" .= imgFormat
    , "source" .= metaSource imgMeta
    , "data"   .= T.decodeUtf8 imgBase64
    ]

data Metadata = Metadata
  { metaSource :: !String
  } 

instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \v -> Metadata
    <$> v .: "source"

fromByteString :: String -> Metadata -> B.ByteString -> Image
fromByteString name meta bs = Image
  { imgName   = name
  , imgMeta   = meta
  , imgBase64 = B.encode bs
  , imgSize   = size
  , imgFormat = format
  }
  where
    size :: (Word, Word)
    size = (getMeta P.Width, getMeta P.Height)

    format :: String
    format = case getMeta P.Format of
      P.SourceBitmap -> "bmp"
      P.SourceJpeg   -> "jpeg"
      P.SourcePng    -> "png"
      P.SourceTiff   -> "tiff"
      x -> error $ "unsupported image format " <> show x

    metadata :: P.Metadatas
    metadata = either (const $ error "could not decode image") snd (P.decodeImageWithMetadata bs)

    getMeta :: P.Keys a -> a
    getMeta = maybe (error "could not read metadata") id . flip P.lookup metadata

data ImageOptions = ImageOptions
  { optSize      :: (Word, Word)
  , optGreyscale :: Bool
  , optBlur      :: Bool
  }

type ImageOutput = (Image, ImageOptions)
type ImageTransfrom = ImageOptions -> ImageOptions

transform :: ImageTransfrom -> Image -> ImageOutput
transform f img = (img, f $ baseOptions img)

resize :: (Word, Word) -> ImageTransfrom
resize size img = img { optSize = size }

greyscale :: ImageTransfrom
greyscale img = img { optGreyscale = True }

blur :: ImageTransfrom
blur img = img { optBlur = True }

baseOptions :: Image -> ImageOptions
baseOptions Image { imgSize } = ImageOptions
  { optSize      = imgSize
  , optGreyscale = False
  , optBlur      = False
  }


aspectRatio' :: Integral a => (a, a) -> Float
aspectRatio' (x, y) = fromIntegral x / fromIntegral y

aspectRatio :: Image -> Float
aspectRatio = aspectRatio' . imgSize

filterSimilarRatio :: Integral a => (a, a) -> Float -> [Image] -> [Image]
filterSimilarRatio (x, y) ratio images = takeWhile inRange matches
  where
    inRange :: Image -> Bool
    inRange img =
      let ar = aspectRatio img
          bm = aspectRatio bestMatch
        in (bm > 1 && bm * (1 - ratio) < ar) || (bm <= 1 && bm * (1 + ratio) > ar)
    compareAspects :: Image -> Float
    compareAspects img = abs $ aspectRatio img - aspectRatio' (x, y)
    matches = sortBy (comparing compareAspects) images
    bestMatch = head matches
