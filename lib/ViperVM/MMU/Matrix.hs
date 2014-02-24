-- | Matrix module
module ViperVM.MMU.Matrix where

import ViperVM.MMU.Data
import ViperVM.MMU.DataType
import ViperVM.STM.TList
import Data.Word

-- | Matrix
data Matrix = Matrix {
   matrixCellType :: MatrixCellType,
   matrixDims :: [Word64],
   matrixInstances :: TList MatrixInstance
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
