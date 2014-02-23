module ViperVM.Platform.OpenCL.Kernel where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library

data Kernel = Kernel Library Kernel_ deriving (Eq)

instance Entity Kernel where 
   unwrap (Kernel _ x) = x
   cllib (Kernel l _) = l
