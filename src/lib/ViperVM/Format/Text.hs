module ViperVM.Format.Text
   ( module Data.Text
   , decodeUtf8
   )
where

import Data.Text
import qualified Data.Text.Encoding as T

import ViperVM.Format.Binary.Buffer

-- | Decode Utf8
decodeUtf8 :: Buffer -> Text
decodeUtf8 (Buffer bs) = T.decodeUtf8 bs
