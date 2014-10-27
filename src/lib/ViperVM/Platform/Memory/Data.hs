-- | Low-level data
module ViperVM.Platform.Memory.Data 
   ( dataCoveringRegion
   , dataCoveringRegion1D
   )
where

import ViperVM.Platform.Types (Data(..))
import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.Region


-- | Return the smallest covering shape
dataCoveringShape :: Data -> Shape
dataCoveringShape = layoutCoveringShape . dataLayout

-- | Return the smallest covering region
dataCoveringRegion :: Data -> Region
dataCoveringRegion d = Region (dataOffset d) (dataCoveringShape d)

-- | Return the smallest covering 1D region
dataCoveringRegion1D :: Data -> Region
dataCoveringRegion1D = regionCover1D . dataCoveringRegion
