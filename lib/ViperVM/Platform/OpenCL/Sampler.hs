module ViperVM.Platform.OpenCL.Sampler (
   Sampler(..)
) where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library

import Control.Monad (void)

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
