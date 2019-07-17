module Waifu.Image
    ( Image(..)
    , Metadata(..)
    , fromByteString
    , resize
    ) where

import qualified Codec.Picture as P
import qualified Codec.Picture.Metadata as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y

data Image = Image
    { imgName   :: String
    , imgMeta   :: Metadata
    , imgBase64 :: B.ByteString
    , imgSize   :: (Word, Word)
    , imgResize :: (Word, Word)
    , imgFormat :: String
    }

data Metadata = Metadata
    { metaSource :: String
    }

instance FromJSON Metadata where
    parseJSON = Y.withObject "metadata" $ \v -> Metadata <$> v .: "source"

fromByteString :: String -> Metadata -> B.ByteString -> Image
fromByteString name meta bs = Image
    { imgName   = name
    , imgMeta   = meta
    , imgBase64 = B.encode bs
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
