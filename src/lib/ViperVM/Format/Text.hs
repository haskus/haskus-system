module ViperVM.Format.Text
   ( module Data.Text
   , bufferDecodeUtf8
   , textEncodeUtf8
   , stringEncodeUtf8
   )
where

import Data.Text
import qualified Data.Text.Encoding as T
import qualified Data.Text          as T

import ViperVM.Format.Binary.Buffer

-- | Decode Utf8
bufferDecodeUtf8 :: Buffer -> Text
bufferDecodeUtf8 (Buffer bs) = T.decodeUtf8 bs

-- | Encode Text into Utf8
textEncodeUtf8 :: Text -> Buffer
textEncodeUtf8 = Buffer . T.encodeUtf8

-- | Encode String into Utf8
stringEncodeUtf8 :: String -> Buffer
stringEncodeUtf8 = textEncodeUtf8 . T.pack
