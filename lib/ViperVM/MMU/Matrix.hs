module ViperVM.MMU.Matrix where

import ViperVM.MMU.Data
import ViperVM.MMU.DataType
import ViperVM.STM.TList
import Data.Word

data Matrix = Matrix {
   matrixCellType :: MatrixCellType,
   matrixDims :: [Word64],
   matrixInstances :: TList MatrixInstance
}

data MatrixCellType = 
     FInt Sign IntBits 
   | FFloat 
   | FDouble

data MatrixInstance = 
   -- | A dense matrix instance is a nested array
   DenseMatrixInstance {
      denseMatrixInstanceData :: Data,
      denseMatrixInstanceDimOrder :: [Int]
   }
