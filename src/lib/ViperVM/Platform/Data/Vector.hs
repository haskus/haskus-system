-- | Vector module
module ViperVM.Platform.Data.Vector 
   ( Vector
   , VectorParameters(..)
   , VectorCellType(..)
   , VectorRepresentation(..)
   , VectorSource(..)
   , new
   , attach
   , attach_
   )
where

import Control.Concurrent.STM
import Control.Monad (void)

import ViperVM.Platform.Types(Data(..))
import ViperVM.Platform.Memory.Layout
import qualified ViperVM.Platform.Memory.Object as MD
import Data.Word

-- | Vector
type Vector = MD.Object VectorParameters VectorRepresentation VectorSource

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
   -- | Sub-vector
   = SourceSubVector
      { sourceSubParent :: Vector   -- ^ Parent
      , sourceSubChild  :: Vector   -- ^ Child
      , sourceSubOffset :: Word64   -- ^ Offset
      , sourceSubSize   :: Word64   -- ^ Size
      }


-- | Create a new Vector
new :: VectorParameters -> STM Vector
new = MD.new

-- | Attach an existing data
attach :: Vector -> VectorRepresentation -> Data -> STM (MD.DataInstance VectorRepresentation)
attach v r d = case r of

   DenseVector -> do
      let 
         vsize = vectorSize (MD.objectParameters v)
         vct = vectorCellType (MD.objectParameters v)

      -- Check layout (1D array with appropriate cell type and size)
      case (vct, dataLayout d) of

         (FFloat, Array (Scalar (FloatField _)) n)
            | n == vsize -> return ()

         (FDouble, Array (Scalar (DoubleField _)) n)
            | n == vsize -> return ()

         (FInt sign bits, Array (Scalar (IntField sign2 bits2 _)) n)
            | n == vsize && sign == sign2 && bits == bits2 -> return ()

         _ -> error "Invalid layout"

      -- Add instance
      MD.addInstance v r d

-- | Attach an existing data (see `attach` if you want to get the instance)
attach_ :: Vector -> VectorRepresentation -> Data -> STM ()
attach_ v r d = void $ attach v r d
