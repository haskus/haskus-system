-- | OpenCL entity module
module ViperVM.Platform.OpenCL.Entity where

import Foreign.Ptr (Ptr)
import ViperVM.Platform.OpenCL.Library

-- | Wrapped OpenCL entity
class Entity e where 
   unwrap :: e -> Ptr ()   -- ^ Get entity pointer
   cllib :: e -> Library   -- ^ Get library associated with the entity
   retain :: e -> IO ()    -- ^ Retain (increment) entity reference counter (if any)
   release :: e -> IO ()   -- ^ Decrement entity reference counter (if any) and release the entity if 0 is reached
