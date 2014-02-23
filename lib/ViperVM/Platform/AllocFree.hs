{-# LANGUAGE ForeignFunctionInterface #-}

-- | Buffer allocation
module ViperVM.Platform.AllocFree where

import Foreign.Ptr (Ptr,nullPtr)
import Foreign.C.Types (CSize(..))

import qualified ViperVM.Platform.OpenCL as CL
import ViperVM.Platform.Types (AllocError(..),BufferSize,
   BufferPeer(..),Memory(..),MemoryPeer(..))


--------------------------------------------------------
-- Host
--------------------------------------------------------

foreign import ccall unsafe "stdlib.h malloc"  malloc :: CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h free"    free   :: Ptr a -> IO ()

-- | Allocate a buffer in host memory
allocateHost :: BufferSize -> Memory -> IO (Either AllocError BufferPeer)
allocateHost size _ = do
   ptr <- malloc (fromIntegral size)
   return $ if ptr == nullPtr
      then Left ErrAllocOutOfMemory
      else Right (HostBuffer ptr)

--------------------------------------------------------
-- OpenCL
--------------------------------------------------------

allocateOpenCL :: BufferSize -> Memory -> IO (Either AllocError BufferPeer)
allocateOpenCL size mem = do
   let 
      peer = memoryPeer mem
      ctx = clMemContext peer
      dev = clMemDevice peer
      flags = []
   buf <- CL.createBuffer dev ctx flags (fromIntegral size)
   
   return $ case buf of
      Right m -> Right (OpenCLBuffer dev ctx m)
      Left _ -> Left ErrAllocUnknown -- FIXME: return appropriate error
