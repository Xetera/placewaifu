module Waifu.Image
    ( Image(..)
    , fromByteString
    , resize
    ) where

import qualified Codec.Picture as P
import qualified Codec.Picture.Metadata as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B

data Image = Image
    { imgBase64 :: B.ByteString
    , imgSize   :: (Word, Word)
    , imgResize :: (Word, Word)
    , imgFormat :: String
    }

fromByteString :: B.ByteString -> Image
fromByteString bs = Image
    { imgBase64 = B.encode bs
    , imgSize   = size
    , imgResize = size
    , imgFormat = format
    }
    where
        size :: (Word, Word)
        size = (getMeta P.Width, getMeta P.Height)

        format :: String
        format = case getMeta P.Format of
            P.SourceJpeg -> "jpeg"
            P.SourcePng  -> "png"
            _ -> "unsupported image format"

        metadata :: P.Metadatas
        metadata = either (const $ error "could not decode image") snd (P.decodeImageWithMetadata bs)

        getMeta :: P.Keys a -> a
        getMeta = maybe (error "could not read metadata") id . flip P.lookup metadata

resize :: (Word, Word) -> Image -> Image
resize size img = img { imgResize = size }
