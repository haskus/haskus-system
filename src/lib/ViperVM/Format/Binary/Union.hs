{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Union (as in C)
--
-- Unions are storable and can contain any storable data. Currently there are
-- Union2, Union3 and Union4 (respectively to contain 2, 3 or 4 members). We
-- could easily extend this to 5+ members.
-- 
-- Use 'fromUnion' to read a alternative:
--
-- @
-- {-# LANGUAGE DataKinds #-}
--
-- getUnion :: IO (Union3 Word16 Word32 Word64)
-- getUnion = ...
--
-- test = do
--    u <- getUnion
--
--    -- to get one of the member
--    let v = fromUnion u :: Word16
--    let v = fromUnion u :: Word32
--    let v = fromUnion u :: Word64
--
--    -- This won't compile (Word8 is not a member of the union)
--    let v = fromUnion u :: Word8
-- @
--
-- Use 'toUnion' to create a new union:
-- @
--
-- let
--    u2 :: Union2 Word32 (Vector 4 Word8)
--    u2 = toUnion (0x12345678 :: Word32)
-- @
module ViperVM.Format.Binary.Union
   ( Union
   , fromUnion
   , toUnion
   )
where

import ViperVM.Utils.Memory (memCopy)

import Data.HList.FakePrelude (ApplyAB(..))
import Data.HList.HList
import Foreign.Storable
import Foreign.CStorable
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

data Union xs = Union !Word64 !(ForeignPtr ()) deriving (Show)

type family IsMember a l :: Bool where
   IsMember a (a ': l) = 'True
   IsMember a (b ': l) = IsMember a l

-- | Retrieve a union member from its index
fromUnion :: (Storable a, IsMember a l ~ 'True) => Union (HList l) -> a
fromUnion (Union _ fp) = unsafePerformIO $ withForeignPtr fp (peek . castPtr)

-- | Create a new union
toUnion :: forall a l . (Storable (Union (HList l)), Storable a, IsMember a l ~ 'True) => a -> Union (HList l)
toUnion v = unsafePerformIO $ do
   fp <- mallocForeignPtr :: IO (ForeignPtr (Union (HList l)))
   withForeignPtr fp $ \p -> 
      poke (castPtr p) v
   return $ Union
      (fromIntegral (sizeOf (undefined :: (Union (HList l)))))
      (castForeignPtr fp)

unionSize :: forall l . HFoldr' SizeOf Int l Int => Union (HList l) -> Int
unionSize _ = hFoldr' SizeOf (0 :: Int) (undefined :: HList l)

unionAlignment :: forall l . HFoldr' Alignment Int l Int => Union (HList l) -> Int
unionAlignment _ = hFoldr' Alignment (0 :: Int) (undefined :: HList l)

data SizeOf    = SizeOf
data Alignment = Alignment

instance (r ~ Int, Storable a) => ApplyAB SizeOf (a, Int) r where
   applyAB _ (_,r) = max r (sizeOf (undefined :: a))

instance (r ~ Int, Storable a) => ApplyAB Alignment (a, Int) r where
   applyAB _ (_,r) = max r (alignment (undefined :: a))


-- | Like HFoldr but only use types, not values!

class HFoldr' f v (l :: [*]) r where
    hFoldr' :: f -> v -> HList l -> r

instance (v ~ v') => HFoldr' f v '[] v' where
    hFoldr'       _ v _   = v

instance (ApplyAB f (e, r) r', HFoldr' f v l r)
    => HFoldr' f v (e ': l) r' where
    hFoldr' f v _ = applyAB f (undefined :: e, hFoldr' f v (undefined :: HList l) :: r)

instance (HFoldr' SizeOf Int l Int,
          HFoldr' Alignment Int l Int
         ) => Storable (Union (HList l)) where
   sizeOf             = unionSize
   alignment          = unionAlignment
   peek ptr = do
      let sz = sizeOf (undefined :: Union (HList l))
      fp <- mallocForeignPtrBytes sz
      withForeignPtr fp $ \p -> 
         memCopy p (castPtr ptr) (fromIntegral sz)
      return (Union (fromIntegral sz) fp)

   poke ptr (Union sz fp) =
      withForeignPtr fp $ \p ->
         memCopy (castPtr ptr) p sz


instance (Storable (Union (HList l))) => CStorable (Union (HList l))  where
   cPeek      = peek
   cPoke      = poke
   cAlignment = alignment
   cSizeOf    = sizeOf


-- TODO: rewrite rules
-- poke p (toUnion x) = poke (castPtr p) x
--
-- (fromUnion <$> peek p) :: IO a  = peek (castPtr p) :: IO a
