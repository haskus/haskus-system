-- | Matrix module
module ViperVM.Platform.Memory.Matrix where

import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.MultiData
import Data.Word

-- | Matrix
type Matrix = MultiData MatrixPrototype MatrixStorage


data MatrixStorage
   = DenseMatrix                       -- ^ Dense array
      { denseMatrixDimOrder :: [Int]   -- ^ Dimensions storage order (e.g. [0,2,1]
      }

data MatrixPrototype = MatrixPrototype
   { matrixCellType :: MatrixCellType
   , matrixDims     :: [Word64]
   }

-- | Matrix cell type
data MatrixCellType = 
     FInt Sign IntBits 
   | FFloat 
   | FDouble
