-- | OpenCL sampler module
module ViperVM.Arch.OpenCL.Sampler
   ( Sampler(..)
   )
where

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library

import Control.Monad (void)

-- | Sampler
data Sampler = Sampler Library Sampler_ deriving (Eq)

instance Entity Sampler where 
   unwrap (Sampler _ x) = x
   cllib (Sampler l _) = l
   retain = retainSampler
   release = releaseSampler

-- | Release a sampler
releaseSampler :: Sampler -> IO ()
releaseSampler ctx = void (rawClReleaseSampler (cllib ctx) (unwrap ctx))

-- | Retain a sampler
retainSampler :: Sampler -> IO ()
retainSampler ctx = void (rawClRetainSampler (cllib ctx) (unwrap ctx))
