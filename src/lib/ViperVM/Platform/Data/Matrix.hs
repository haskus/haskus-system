-- | Matrix module
module ViperVM.Platform.Data.Matrix 
   ( Matrix
   , MatrixParameters(..)
   , MatrixCellType(..)
   , MatrixRepresentation(..)
   , MatrixSource(..)
   , new
   )
where

import Control.Concurrent.STM

import ViperVM.Platform.Memory.Layout
import qualified ViperVM.Platform.Memory.Object as MD
import ViperVM.Format.Binary.Word

-- | Matrix
type Matrix = MD.Object MatrixParameters MatrixRepresentation MatrixSource

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
   -- | Dense array
   = DenseMatrix
      { denseMatrixDimOrder :: [Int]   -- ^ Dimensions storage order (e.g. [0,2,1]
      }



-- | Matrix creation from another data
data MatrixSource
   -- | Sub-matrix (same number of dimensions)
   = SourceSubMatrix
      { sourceSubParent :: Matrix   -- ^ Parent matrix
      , sourceSubChild  :: Matrix   -- ^ Child matrix
      , sourceSubOffset :: [Word64] -- ^ Offset of the first element
      , sourceSubDims   :: [Word64] -- ^ Dimensions
      }

-- | Create a new Matrix
new :: MatrixParameters -> STM Matrix
new = MD.new
