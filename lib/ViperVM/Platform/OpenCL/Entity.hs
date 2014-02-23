module ViperVM.Platform.OpenCL.Entity where

import Foreign.Ptr (Ptr)
import ViperVM.Platform.OpenCL.Library

class Entity e where 
   unwrap :: e -> Ptr ()
   cllib :: e -> Library
