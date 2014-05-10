module ViperVM.Platform.Drivers.Host (
   Memory(..), Buffer(..), Proc(..),
   allocateBuffer, releaseBuffer
) where

import Foreign.Ptr (Ptr)
import Data.Word (Word,Word64)
import Control.Applicative ((<$>))

import ViperVM.Arch.Common.Endianness
import ViperVM.Arch.Common.Errors
import qualified ViperVM.Arch.Posix.Malloc as Posix

data Memory = Memory {
   hostMemSize :: Word64,
   hostMemEndianness :: Endianness
}

data Buffer = Buffer {
  hostBufferPtr :: Ptr ()
} deriving (Eq)

data Proc = Proc {
   cpuIndex :: Word
}

-- | Allocate a buffer in host memory
allocateBuffer :: Word64 -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size _ = fmap Buffer <$> Posix.malloc (fromIntegral size)

releaseBuffer :: Memory -> Buffer -> IO ()
releaseBuffer _ buf = Posix.free (hostBufferPtr buf)
