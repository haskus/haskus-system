-- | Matrix module
module ViperVM.Platform.Memory.Matrix where

import ViperVM.Platform.Memory.Data
import ViperVM.Platform.Memory.Layout
import Data.Word

-- | Matrix
data Matrix = Matrix {
   matrixCellType :: MatrixCellType,
   matrixDims :: [Word64]
--   matrixInstances :: TSet MatrixInstance
}

-- | Matrix cell type
data MatrixCellType = 
     FInt Sign IntBits 
   | FFloat 
   | FDouble

-- | Matrix instance
data MatrixInstance = 
   -- | A dense matrix instance is a nested array
   DenseMatrixInstance {
      denseMatrixInstanceData :: Data,
      denseMatrixInstanceDimOrder :: [Int]
   }
