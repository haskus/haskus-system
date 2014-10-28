-- | Matrix module
module ViperVM.Platform.Data.Matrix where

import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.MultiData
import Data.Word

-- | Matrix
type Matrix = MultiData MatrixParameters MatrixRepresentation MatrixSource

-- | Matrix parameters
data MatrixParameters = MatrixParameters
   { matrixCellType :: MatrixCellType     -- Cell type
   , matrixDims     :: [Word64]           -- Dimensions
   }

-- | Matrix cell type
data MatrixCellType = 
     FInt Sign IntBits 
   | FFloat 
   | FDouble

-- | Matrix representations
data MatrixRepresentation
   = DenseMatrix                       -- ^ Dense array
      { denseMatrixDimOrder :: [Int]   -- ^ Dimensions storage order (e.g. [0,2,1]
      }



-- | Matrix creation from another data
data MatrixSource
   = SourceSubMatrix                -- ^ Sub-matrix (same number of dimensions)
      { sourceSubParent :: Matrix   -- ^ Parent matrix
      , sourceSubChild  :: Matrix   -- ^ Child matrix
      , sourceSubOffset :: [Word64] -- ^ Offset of the first element
      , sourceSubDims   :: [Word64] -- ^ Dimensions
      }
