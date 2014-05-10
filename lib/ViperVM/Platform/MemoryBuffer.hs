module ViperVM.Platform.MemoryBuffer (
   MemoryBuffer(..)
) where

import ViperVM.Platform.Topology (Memory)
import ViperVM.Platform.Memory.Buffer (Buffer)

-- | Memory buffer
data MemoryBuffer = MemoryBuffer Memory Buffer deriving (Eq)
