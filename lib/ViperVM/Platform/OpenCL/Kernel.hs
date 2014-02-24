-- | OpenCL kernel module
module ViperVM.Platform.OpenCL.Kernel (
   Kernel(..)
) where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library

import Control.Monad (void)

-- | Kernel
data Kernel = Kernel Library Kernel_ deriving (Eq)

instance Entity Kernel where 
   unwrap (Kernel _ x) = x
   cllib (Kernel l _) = l
   retain = retainKernel
   release = releaseKernel

-- | Release a kernel
releaseKernel :: Kernel -> IO ()
releaseKernel ctx = void (rawClReleaseKernel (cllib ctx) (unwrap ctx))

-- | Retain a kernel
retainKernel :: Kernel -> IO ()
retainKernel ctx = void (rawClRetainKernel (cllib ctx) (unwrap ctx))
