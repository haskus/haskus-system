{-# LANGUAGE TemplateHaskell #-}

-- | Computational kernel in the kernel library
module ViperVM.Library.Kernel
   ( Kernel(..)
   , KernelBin(..)
   , kernelHash
   )
where

import Data.ByteString as SBS
import Data.SafeCopy

import ViperVM.Utils.Hash (hashString)

-- | Various Kernel representations
data Kernel
   = OpenCLSource String   -- ^ OpenCL kernel source code

-- | COmpiled kernel
data KernelBin
   = OpenCLBinary SBS.ByteString   -- ^ OpenCL kernel program binary

-- KernelBin can be stored on disk for reuse
deriveSafeCopy 1 'base ''KernelBin

-- | Compute a hash of the kernel
kernelHash :: Kernel -> String
kernelHash (OpenCLSource src) = "clsrc_" ++ hashString src
