module ViperVM.Platform.OpenCL.Sampler where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library

data Sampler = Sampler Library Sampler_ deriving (Eq)

instance Entity Sampler where 
   unwrap (Sampler _ x) = x
   cllib (Sampler l _) = l
