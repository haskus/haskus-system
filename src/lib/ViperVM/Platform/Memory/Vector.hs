-- | Vector module
module ViperVM.Platform.Memory.Vector where

import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.MultiData
import Data.Word

-- | Vector
type Vector = MultiData VectorPrototype VectorStorage


data VectorStorage = DenseVector

data VectorPrototype = VectorPrototype
   { vectorCellType :: VectorCellType
   , vectorSize     :: Word64
   }

-- | Vector cell type
data VectorCellType = 
     FInt Sign IntBits 
   | FFloat 
   | FDouble
