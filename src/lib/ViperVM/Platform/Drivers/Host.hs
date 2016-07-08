-- | Host driver
module ViperVM.Platform.Drivers.Host
   ( Memory(..)
   , Buffer(..)
   , Proc(..)
   , hostMemUID
   , hostProcUID
   , hostProcModel
   , hostBufferUID
   , allocateBuffer
   , releaseBuffer
   , registerBuffer
   )
where

import Data.Ord (comparing)

import ViperVM.Format.Binary.Ptr (Ptr,castPtr)
import ViperVM.Format.Binary.Endianness
import ViperVM.Format.Binary.Word
import qualified ViperVM.Format.Text as Text
import ViperVM.Arch.Common.Errors
import qualified ViperVM.Arch.Posix.Malloc as Posix
import qualified ViperVM.Arch.X86_64.Cpuid as C

data Memory = Memory
   { hostMemNode :: Word
   , hostMemSize :: Word64
   , hostMemEndianness :: Endianness
   }

instance Eq Memory where
   (==) a b = hostMemNode a == hostMemNode b

instance Ord Memory where
   compare = comparing hostMemNode

data Buffer = Buffer 
   { hostBufferSize :: Word64
   , hostBufferPtr :: Ptr ()
   } deriving (Eq,Ord)

data Proc = Proc
   { hostProcNode :: Word
   , hostProcIndex :: Word
   } deriving (Eq,Ord)

-- | Unique memory ID
hostMemUID :: Memory -> String
hostMemUID _ = "Host Memory"

-- | Unique proc ID
hostProcUID :: Proc -> String
hostProcUID p = Text.printf "Host Proc %d:%d" (hostProcNode p) (hostProcIndex p)

-- | Processor model
hostProcModel :: Proc -> String
hostProcModel _ = Text.printf "%s - %s" (Text.unpack C.procVendor) (Text.unpack C.procBrand)

-- | Unique buffer ID
hostBufferUID :: Buffer -> String
hostBufferUID buf = Text.printf "Host Buffer %s" (show . hostBufferPtr $ buf)

-- | Allocate a buffer in host memory
allocateBuffer :: Word64 -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size _ = fmap (Buffer size) <$> Posix.malloc (fromIntegral size)

-- | Release a buffer in host memory
releaseBuffer :: Memory -> Buffer -> IO ()
releaseBuffer _ buf = Posix.free (hostBufferPtr buf)


-- | Convert a pointer into a buffer
registerBuffer :: Memory -> Ptr a -> Word64 -> Buffer
registerBuffer _ ptr size = Buffer size (castPtr ptr)
