module ViperVM.MMU.Data (
   Data(..),
   coarseRegionFromData
) where

import ViperVM.MMU.DataType
import ViperVM.MMU.Region
import ViperVM.Platform.Platform (Buffer)

-- | A data in a memory
data Data = Data DataType Offset Buffer

-- | Return a coarse region encompassing the data
coarseRegionFromData :: Data -> Region
coarseRegionFromData (Data typ off _) = coarseRegionFromDataType typ off
