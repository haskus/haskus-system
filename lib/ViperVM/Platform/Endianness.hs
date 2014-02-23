module ViperVM.Platform.Endianness (
   getMemoryEndianness,
   getOpenCLDeviceEndianness
) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Data.Bits ((.|.), shiftL)
import Data.Word (Word8, Word32)
import Foreign.Ptr (castPtr, Ptr)
import Foreign.Storable (poke)
import Control.Applicative ((<$>))

import ViperVM.Platform.Types (Endianness(..))

import qualified ViperVM.Platform.OpenCL as CL

-- | Indicate the endianness of the host memory
getMemoryEndianness :: IO Endianness
getMemoryEndianness = do
   -- Write a 32 bit Int and check byte ordering
   let magic = 1 .|. (shiftL 8 2) .|. (shiftL 16 3) .|. (shiftL 24 4) :: Word32
   alloca $ \p -> do
      poke p magic
      rs <- peekArray 4 (castPtr p :: Ptr Word8)
      return $ if rs == [1,2,3,4] then BigEndian else LittleEndian


-- | Get endianness of an OpenCL device
getOpenCLDeviceEndianness :: CL.Device -> IO Endianness
getOpenCLDeviceEndianness dev = toEndianness <$> CL.isDeviceLittleEndian' dev
   where
      toEndianness True = LittleEndian
      toEndianness False = BigEndian
