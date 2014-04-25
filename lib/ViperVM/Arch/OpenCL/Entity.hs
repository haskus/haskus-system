-- | OpenCL entity module
module ViperVM.Arch.OpenCL.Entity where

import Foreign.Ptr (Ptr)
import ViperVM.Arch.OpenCL.Library

-- | Wrapped OpenCL entity
class Entity e where 
   -- | Get entity pointer
   unwrap :: e -> Ptr ()
   -- | Get library associated with the entity
   cllib :: e -> Library
   -- | Retain (increment) entity reference counter (if any)
   retain :: e -> IO ()
   -- | Decrement entity reference counter (if any) and release the entity if 0 is reached
   release :: e -> IO ()
