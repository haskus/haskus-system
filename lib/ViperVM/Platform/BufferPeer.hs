module ViperVM.Platform.BufferPeer (
   BufferPeer(..)
) where

import Foreign.Ptr (Ptr)

import qualified ViperVM.Arch.OpenCL.All as CL

-- | Backend specific buffer fields
data BufferPeer = 
     HostBuffer (Ptr ())
   | CUDABuffer
   | OpenCLBuffer CL.Device CL.Context CL.Mem
   | DiskBuffer
   deriving (Eq)

