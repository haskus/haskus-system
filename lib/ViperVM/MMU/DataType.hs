-- | Low-level data types
module ViperVM.MMU.DataType (
   DataType(..), ScalarType(..), 
   Sign(..), IntBits(..),
   SizeOf, packedSizeOf, fieldType, fieldOffset,
   coarseRegionFromDataType,
   coveringRegionFromDataType
) where

import Prelude hiding (sum)
import Data.Word
import Data.Foldable (sum)
import qualified Data.Vector as V
import Control.Applicative ((<$>))
import ViperVM.MMU.Region
import ViperVM.Platform.Types (Endianness)

-- | A deterministic low-level data-type
--
-- Deterministic because we do not support unions or other data-dependent types
data DataType = 
     Scalar ScalarType           -- ^ Scalar value
   | Array DataType ArraySize    -- ^ Array with determined size
   | Struct (V.Vector DataType)           -- ^ Regular structure; the order of the list of fields matters
   | Padding Word64              -- ^ Padding bytes (they have no sense for this data)
   deriving (Show)

type ArraySize = Word64                      -- ^ Size of an array in cells
data Sign = Signed | Unsigned                -- ^ Sign of an integer
            deriving (Show)
data IntBits = Bit8 | Bit16 | Bit32 | Bit64  -- ^ Number of bits representing an integer
               deriving (Show)

-- | Scalar type
data ScalarType =
     TInt Sign IntBits Endianness
   | TFloat Endianness
   | TDouble Endianness
   deriving (Show)

-- | Data type with a fixed number of bytes to represent it
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
   sizeOf (Struct ts) = sum $ sizeOf <$> ts

-- | Return the field type according to the given field path
--
-- Field index into array are not taken into account,
-- i.e. fieldType [0,m] (Struct [Array X n]) will return X for any (n,m)
--
fieldType :: [Word64] -> DataType -> DataType
fieldType [] dt = dt
fieldType (x:xs) dt = case dt of
   Struct fs -> fieldType xs (fs V.! fromIntegral x)
   Array ct _ -> fieldType xs ct
   _ -> error "Invalid field path"

-- | Return field offset for the given path
fieldOffset :: [Word64] -> DataType -> Offset
fieldOffset = go 0
   where
      go off [] _ = off
      go off (x:xs) dt = case dt of
         Struct fs -> go off' xs (fs V.! fromIntegral x)
            where off' = off + (sum . fmap sizeOf . V.take (fromIntegral x + 1) $ fs)
         Array ct n 
            | x < n -> go (off + x*sizeOf ct) xs ct
            | otherwise -> error "Invalid array indexing"
         _ -> error "Invalid field path"

-- | Return the size of a data type without padding bytes
packedSizeOf :: DataType -> Word64
packedSizeOf dt = case dt of
   s@(Scalar {}) -> sizeOf s
   Padding _ -> 0
   Array dt' n -> n * packedSizeOf dt'
   Struct dts -> sum (fmap packedSizeOf dts)


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

   Array (Struct fs) n -> reg where
      (useful,padding) = stripStructPadding (V.toList fs)
      reg = if padding == 0
         then Region1D off (n * useful)
         else Region2D off n useful padding

   Array t' n -> Region1D off (n * sizeOf t')

   Struct ts -> Region1D off (fst $ stripStructPadding (V.toList ts))


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
