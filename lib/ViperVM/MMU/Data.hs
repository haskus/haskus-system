-- | Low-level data
module ViperVM.MMU.Data (
   Data(..),
   coveringRegion, coveringRegion1D,
   allocateData, allocateDataWithEndianness
) where

import Data.Word (Word64)
import qualified Data.Vector as V
import Control.Applicative ((<$>))

import ViperVM.Arch.Common.Endianness
import ViperVM.MMU.FieldMap
import ViperVM.MMU.Region
import ViperVM.Platform (
   Buffer, Memory, AllocError, 
   allocateBuffer, memoryEndianness)

-- | A data in a buffer
data Data = Data {
   dataType :: FieldMap,
   dataOffset :: Offset,
   dataBuffer :: Buffer
}

-- | Return the smallest covering 1D region
coveringRegion1D :: Data -> Region
coveringRegion1D = regionCover . coveringRegion


-- | Return the smallest covering region (or 1D region if it is the best solution)
--
-- Only the last padding bytes of structures in outermost arrays is deleted to
-- build 2D regions.
coveringRegion :: Data -> Region
coveringRegion d = 
   let 
      off = dataOffset d 
   in 
      case dataType d of
         Scalar x  -> Region1D off (sizeOf x)

         Padding _ -> Region1D off 0

         Array s@(Struct {}) n -> reg where
            (useful,padding) = stripStructPadding s
            reg = if padding == 0
               then Region1D off (n * useful)
               else Region2D off n useful padding

         Array t' n -> Region1D off (n * sizeOf t')

         t@(Struct {}) -> Region1D off (fst $ stripStructPadding t)


-- | Return (useful,padding) where `padding` is the number
-- of padding bytes at the end of the structure and `useful`
-- the number of remaining bytes (useful ones and other padding)
stripStructPadding :: FieldMap -> (Word64,Word64)
stripStructPadding (Struct ts) = (useful,padding)
   where
      (useful,padding) = V.foldr f (0,0) ts
      f (Padding p') (0,p) = (0,p'+p)
      f t (u,p) = (u+sizeOf t, p)
stripStructPadding t = (sizeOf t, 0)


-- | Allocate a data in a memory
allocateData :: FieldMap -> Memory -> IO (Either AllocError Data)
allocateData fm mem = fmap (Data fm 0) <$> allocateBuffer (sizeOf fm) mem

-- | Allocate a data in a memory, passing endianness as a parameter for field map construction
allocateDataWithEndianness :: (Endianness -> FieldMap) -> Memory -> IO (Either AllocError Data)
allocateDataWithEndianness f mem = fmap (Data fm 0) <$> allocateBuffer (sizeOf fm) mem
   where
      fm = f (memoryEndianness mem)
