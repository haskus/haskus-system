-- | OpenCL kernel module
module ViperVM.Arch.OpenCL.Kernel
   ( Kernel(..)
   )
where

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library

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
