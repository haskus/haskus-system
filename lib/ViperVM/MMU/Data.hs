-- | Low-level data
module ViperVM.MMU.Data (
   Data(..),
   coarseRegionFromData
) where

import ViperVM.MMU.DataType
import ViperVM.MMU.Region
import ViperVM.Platform.Types (Buffer)

-- | A data in a buffer
data Data = Data DataType Offset Buffer

-- | Return a coarse region encompassing the data
coarseRegionFromData :: Data -> Region
coarseRegionFromData (Data typ off _) = coarseRegionFromDataType typ off
