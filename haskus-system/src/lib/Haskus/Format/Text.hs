module Haskus.Format.Text
   ( module Data.Text
   -- * Conversions
   , bufferDecodeUtf8
   , textEncodeUtf8
   , stringEncodeUtf8
   -- * Formatting
   , textFormat
   , F.Format
   , (F.%)
   , (F.%.)
   , module Formatting.Formatters
   -- * Parsing
   , textParseHexadecimal
   -- * Get/Put
   , putTextUtf8
   , getTextUtf8
   , getTextUtf8Nul
   , tshow
   -- * IO
   , T.putStrLn
   )
where

import Data.Text hiding (center)
import qualified Data.Text.Encoding as T
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import Formatting                   as F
import Formatting.Formatters
import Data.Text.Read               as T

import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Put
import Haskus.Format.Binary.Get

-- | Decode Utf8
bufferDecodeUtf8 :: Buffer -> Text
bufferDecodeUtf8 (Buffer bs) = T.decodeUtf8 bs

-- | Encode Text into Utf8
textEncodeUtf8 :: Text -> Buffer
textEncodeUtf8 = Buffer . T.encodeUtf8

-- | Encode String into Utf8
stringEncodeUtf8 :: String -> Buffer
stringEncodeUtf8 = textEncodeUtf8 . T.pack

-- | Format a text (strict)
textFormat :: Format Text a -> a
textFormat = F.sformat

-- | Parse an hexadecimal number
-- FIXME: use a real parser (MegaParsec, etc.)
textParseHexadecimal :: Integral a => Text -> Either String a
textParseHexadecimal s = fst <$> T.hexadecimal s

-- | Put an UTF8 encoded text
putTextUtf8 :: Text -> Put
putTextUtf8 = putBuffer . textEncodeUtf8

-- | Pull n bytes from the input, as a Buffer
getTextUtf8 :: Word -> Get Text
getTextUtf8 sz = bufferDecodeUtf8 <$> getBuffer sz

-- | Pull \0 terminal text
getTextUtf8Nul :: Get Text
getTextUtf8Nul = bufferDecodeUtf8 <$> getBufferNul

-- | Show as Text
tshow :: Show a => a -> Text
tshow = pack . show
