-- | Low-level data
module ViperVM.MMU.Data (
   Data(..),
   coarseRegionFromData
) where

import ViperVM.MMU.FieldMap
import ViperVM.MMU.Region
import ViperVM.Platform.Types (Buffer)

-- | A data in a buffer
data Data = Data {
   dataType :: FieldMap,
   dataOffset :: Offset,
   dataBuffer :: Buffer
}

-- | Return a coarse region encompassing the data
coarseRegionFromData :: Data -> Region
coarseRegionFromData (Data typ off _) = englobingCoarseRegion typ off
