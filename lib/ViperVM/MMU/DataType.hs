module ViperVM.MMU.DataType (
   DataType(..), ScalarType(..), 
   Sign(..), IntBits(..),
   SizeOf,
   coarseRegionFromDataType,
   coveringRegionFromDataType
) where

import Data.Word
import Control.Applicative ((<$>))
import ViperVM.MMU.Region
import ViperVM.Platform.Endianness (Endianness)

type ArraySize = Word64

data DataType = 
     Scalar ScalarType
   | Padding Word64
   | Array DataType ArraySize
   | Struct [DataType]

data Sign = Signed | Unsigned
data IntBits = Bit8 | Bit16 | Bit32 | Bit64

data ScalarType =
     TInt Sign IntBits Endianness
   | TFloat Endianness
   | TDouble Endianness


class SizeOf t where
   sizeOf :: t -> Word64

instance SizeOf ScalarType where
   sizeOf (TInt _ Bit8  _) = 1
   sizeOf (TInt _ Bit16 _) = 2
   sizeOf (TInt _ Bit32 _) = 4
   sizeOf (TInt _ Bit64 _) = 8
   sizeOf (TFloat {})      = 4
   sizeOf (TDouble {})     = 8

instance SizeOf DataType where
   sizeOf (Scalar x) = sizeOf x
   sizeOf (Padding n) = n
   sizeOf (Array t n) = n * sizeOf t
   sizeOf (Struct ts) = foldl(+) 0 (sizeOf <$> ts)

-- | Return coarse region containing effective data
--
-- Only the last padding bytes of structures in outermost arrays is deleted to
-- build 2D regions.
--
-- Useful for data transfers (especially if strided transfers are supported)
coarseRegionFromDataType :: DataType -> Offset -> Region
coarseRegionFromDataType t off = case t of
   Scalar x  -> Region1D off (sizeOf x)
   Padding _ -> Region1D off 0
   Array (Struct []) _ -> Region1D off 0
   Array (Struct ts) n -> let (useful,padding) = stripStructPadding ts in
      if padding == 0 
         then Region1D off (n * useful)
         else Region2D off n useful padding
   Array t' n -> Region1D off (n * sizeOf t')
   Struct ts -> Region1D off (fst $ stripStructPadding ts)


-- | Return covering 1D region from data type
--
-- Useful for buffer allocation from data type
coveringRegionFromDataType :: DataType -> Offset -> Region
coveringRegionFromDataType dt off = regionCover (coarseRegionFromDataType dt off)

-- | Return (useful,padding) where `padding` is the number
-- of padding bytes at the end of the structure and `useful`
-- the number of remaining bytes (useful ones and other padding)
stripStructPadding :: [DataType] -> (Word64,Word64)
stripStructPadding ts = (useful,padding)
   where
      (useful,padding) = foldr f (0,0) ts
      f (Padding p') (0,p) = (0,p'+p)
      f t (u,p) = (u+sizeOf t, p)
