-- | Put monad
module ViperVM.Format.Binary.Put
   ( module Data.Serialize.Put
   , putBuffer
   , putTextUtf8
   , putPadding
   , putPaddingAlign
   )
where

import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Control.Monad (replicateM_)
import Data.Serialize.Put

import ViperVM.Format.Binary.Buffer

-- | Put a buffer
putBuffer :: Buffer -> Put
putBuffer (Buffer bs) = putByteString bs

-- | Put the given text
putTextUtf8 :: Text -> Put
putTextUtf8 = putByteString . Text.encodeUtf8

-- | Put null bytes
putPadding :: Word -> Put
putPadding n = replicateM_ (fromIntegral n) (putWord8 0x00)

-- | Put null bytes to align the given value to the second
putPaddingAlign :: Word -> Word -> Put
putPaddingAlign n al = putPadding n'
   where
      n' = case n `mod` al of
               0 -> 0
               x -> al - fromIntegral x

