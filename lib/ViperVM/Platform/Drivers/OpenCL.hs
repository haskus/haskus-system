module ViperVM.Platform.Drivers.OpenCL (
   Memory(..), Buffer(..), Network(..), Proc(..),
   allocateBuffer, releaseBuffer
) where

import Data.Word (Word64)
import Data.Ord (comparing)

import ViperVM.Arch.Common.Endianness
import ViperVM.Arch.Common.Errors
import qualified ViperVM.Arch.OpenCL.All as CL

data Memory = Memory {
   clMemLibrary :: CL.Library,
   clMemDevice :: CL.Device,
   clMemContext :: CL.Context,
   clMemEndianness :: Endianness,
   clMemSize :: Word64
}

instance Eq Memory where
   (==) a b = clMemDevice a == clMemDevice b

instance Ord Memory where
   compare = comparing clMemDevice

data Buffer = Buffer {
   clBufferDevice :: CL.Device,
   clBufferContext :: CL.Context,
   clBufferPeer :: CL.Mem
} deriving (Eq)

instance Ord Buffer where
   compare = comparing clBufferPeer

data Network = Network {
   clLinkDevice :: CL.Device,
   clLinkContext :: CL.Context,
   clLinkQueue :: CL.CommandQueue
}

instance Eq Network where
   (==) a b = clLinkDevice a == clLinkDevice b

instance Ord Network where
   compare = comparing clLinkDevice

data Proc = Proc {
   clProcDevice :: CL.Device,
   clProcContext :: CL.Context
}

instance Eq Proc where
   (==) a b = clProcDevice a == clProcDevice b

instance Ord Proc where
   compare = comparing clProcDevice

-- | Allocate a buffer in OpenCL memory
allocateBuffer :: Word64 -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size mem = do
   let 
      ctx = clMemContext mem
      dev = clMemDevice mem
      flags = []
   buf <- CL.createBuffer dev ctx flags (fromIntegral size)
   
   return $ case buf of
      Right m -> Right (Buffer dev ctx m)
      Left _ -> Left AllocUnknownError -- FIXME: return appropriate error

-- | Release a buffer in OpenCL memory
releaseBuffer :: Memory -> Buffer -> IO ()
releaseBuffer _ buf = CL.release (clBufferPeer buf)
