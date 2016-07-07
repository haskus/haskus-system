-- | Put monad
module ViperVM.Format.Binary.Put
   ( Put
   , runPut
   -- * Put
   , putBuffer
   , putByteString
   , putTextUtf8
   , putPadding
   , putPaddingAlign
   , putWord8
   , putWord16le
   , putWord16be
   , putWord32le
   , putWord32be
   , putWord64le
   , putWord64be
   )
where

import qualified Data.ByteString as BS
import qualified Data.Serialize.Put as BP
import Data.Serialize.Put (Put)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Control.Monad (replicateM_)

import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Word

-- | Execute Put
runPut :: Put -> Buffer
runPut = Buffer . BP.runPut

-- | Put a buffer
putBuffer :: Buffer -> Put
putBuffer (Buffer bs) = BP.putByteString bs

-- | Put a ByteString
putByteString :: BS.ByteString -> Put
putByteString = BP.putByteString

-- | Put the given text
putTextUtf8 :: Text -> Put
putTextUtf8 = BP.putByteString . Text.encodeUtf8

-- | Put null bytes
putPadding :: Word -> Put
putPadding n = replicateM_ (fromIntegral n) (BP.putWord8 0x00)

-- | Put null bytes to align the given value to the second
putPaddingAlign :: Word -> Word -> Put
putPaddingAlign n al = putPadding n'
   where
      n' = case n `mod` al of
               0 -> 0
               x -> al - fromIntegral x

-- | Put a Word8
putWord8 :: Word8 -> Put
putWord8 = BP.putWord8

-- | Put a Word16 little-endian
putWord16le :: Word16 -> Put
putWord16le = BP.putWord16le

-- | Put a Word16 big-endian
putWord16be :: Word16 -> Put
putWord16be = BP.putWord16be

-- | Put a Word32 little-endian
putWord32le :: Word32 -> Put
putWord32le = BP.putWord32le

-- | Put a Word32 big-endian
putWord32be :: Word32 -> Put
putWord32be = BP.putWord32be

-- | Put a Word64 little-endian
putWord64le :: Word64 -> Put
putWord64le = BP.putWord64le

-- | Put a Word64 big-endian
putWord64be :: Word64 -> Put
putWord64be = BP.putWord64be
