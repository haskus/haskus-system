-- | Low-level data
module ViperVM.Platform.Memory.Data (
   Data(..), BufferData(..),
   dataCoveringRegion, dataCoveringRegion1D
) where

import Data.Word (Word64)

import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.Region
import ViperVM.Platform.MemoryBuffer

-- | A data physically stored in memory with the given layout
data Data = Data {
   dataOffset :: Word64,
   dataLayout :: Layout
}

-- | A data associated with its buffer
data BufferData = BufferData {
   bufferDataBuffer :: MemoryBuffer,
   bufferDataData :: Data
}

-- | Return the smallest covering shape
dataCoveringShape :: Data -> Shape
dataCoveringShape = layoutCoveringShape . dataLayout

-- | Return the smallest covering region
dataCoveringRegion :: Data -> Region
dataCoveringRegion d = Region (dataOffset d) (dataCoveringShape d)

-- | Return the smallest covering 1D region
dataCoveringRegion1D :: Data -> Region
dataCoveringRegion1D = regionCover1D . dataCoveringRegion
