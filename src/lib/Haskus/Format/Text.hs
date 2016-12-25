module Haskus.Format.Text
   ( module Data.Text
   -- * Conversions
   , bufferDecodeUtf8
   , textEncodeUtf8
   , stringEncodeUtf8
   -- * Formatting
   , textFormat
   , T.Only (..)
   , T.hex
   , printf
   -- * Parsing
   , textParseHexadecimal
   -- * IO
   , T.putStrLn
   )
where

import Data.Text
import qualified Data.Text.Encoding as T
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import Data.Text.Lazy (toStrict)
import Data.Text.Format             as T
import Data.Text.Format.Params      as T
import Data.Text.Read               as T
import Text.Printf

import Haskus.Format.Binary.Buffer

-- | Decode Utf8
bufferDecodeUtf8 :: Buffer -> Text
bufferDecodeUtf8 (Buffer bs) = T.decodeUtf8 bs

-- | Encode Text into Utf8
textEncodeUtf8 :: Text -> Buffer
textEncodeUtf8 = Buffer . T.encodeUtf8

-- | Encode String into Utf8
stringEncodeUtf8 :: String -> Buffer
stringEncodeUtf8 = textEncodeUtf8 . T.pack

-- | Format a text
textFormat :: (T.Params ps) => Format -> ps -> Text
textFormat fmt ps = toStrict (T.format fmt ps)

-- | Parse an hexadecimal number
-- FIXME: use a real parser (MegaParsec, etc.)
textParseHexadecimal :: Integral a => Text -> Either String a
textParseHexadecimal s = fst <$> T.hexadecimal s
