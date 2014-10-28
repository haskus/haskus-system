-- | Vector module
module ViperVM.Platform.Data.Vector where

import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.MultiData
import Data.Word

-- | Vector
type Vector = MultiData VectorParameters VectorRepresentation VectorSource

-- | Data parameters
data VectorParameters = VectorParameters
   { vectorCellType :: VectorCellType     -- Element type
   , vectorSize     :: Word64             -- Number of cells
   }

-- | Vector cell type
data VectorCellType = 
     FInt Sign IntBits 
   | FFloat 
   | FDouble

-- | Vector representation
data VectorRepresentation = DenseVector



-- | Vector creation from another data
data VectorSource
   = SourceSubVector                -- ^ Sub-vector
      { sourceSubParent :: Vector   -- ^ Parent
      , sourceSubChild  :: Vector   -- ^ Child
      , sourceSubOffset :: Word64   -- ^ Offset
      , sourceSubSize   :: Word64   -- ^ Size
      }
