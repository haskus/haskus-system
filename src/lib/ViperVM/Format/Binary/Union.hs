{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Union (as in C)
--
-- Unions are storable and can contain any storable data.
-- 
-- Use 'fromUnion' to read a alternative:
--
-- @
-- {-# LANGUAGE DataKinds #-}
--
-- getUnion :: IO (Union '[Word16 Word32 Word64])
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
--    u2 :: Union '[Word32, Vector 4 Word8]
--    u2 = toUnion (0x12345678 :: Word32)
-- @
module ViperVM.Format.Binary.Union
   ( Union
   , fromUnion
   , toUnion
   , toUnionZero
   )
where

import ViperVM.Utils.Memory (memCopy, memSet)
import ViperVM.Utils.HList

import Data.HList.FakePrelude (ApplyAB(..))
import Data.HList.HList
import Foreign.Storable
import Foreign.CStorable
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)

-- | An union 
--
-- We use a list of types as a parameter.
--
-- The union is just a pointer to a buffer containing the value(s). The size of
-- the buffer is implicitly known from the types in the list.
newtype Union (x :: [*]) = Union (ForeignPtr ()) deriving (Show)

-- | Retrieve a union member from its type
fromUnion :: (Storable a, IsMember a l ~ 'True) => Union l -> a
fromUnion (Union fp) = unsafePerformIO $ withForeignPtr fp (peek . castPtr)

-- | Create a new union from one of the union types
toUnion :: forall a l . (Storable (Union l), Storable a, IsMember a l ~ 'True) => a -> Union l
toUnion = toUnion' False

-- | Like 'toUnion' but set the remaining bytes to 0
toUnionZero :: forall a l . (Storable (Union l), Storable a, IsMember a l ~ 'True) => a -> Union l
toUnionZero = toUnion' True


-- | Create a new union from one of the union types
toUnion' :: forall a l . (Storable (Union l), Storable a, IsMember a l ~ 'True) => Bool -> a -> Union l
toUnion' zero v = unsafePerformIO $ do
   let sz = sizeOf (undefined :: Union l)
   fp <- mallocForeignPtrBytes sz
   withForeignPtr fp $ \p -> do
      -- set bytes after the object to 0
      when zero $ do
         let psz = sizeOf (undefined :: a)
         memSet (p `plusPtr` psz) (fromIntegral (sz - psz)) 0
      poke (castPtr p) v
   return $ Union fp


-------------------------------------------------------------------------------------
-- We use HFoldr' to get the maximum size and alignment of the types in the union
-------------------------------------------------------------------------------------

data SizeOf    = SizeOf
data Alignment = Alignment

instance (r ~ Int, Storable a) => ApplyAB SizeOf (a, Int) r where
   applyAB _ (_,r) = max r (sizeOf (undefined :: a))

instance (r ~ Int, Storable a) => ApplyAB Alignment (a, Int) r where
   applyAB _ (_,r) = max r (alignment (undefined :: a))

-- | Get the union size (i.e. the maximum of the types in the union)
unionSize :: forall l . HFoldr' SizeOf Int l Int => Union l -> Int
unionSize _ = hFoldr' SizeOf (0 :: Int) (undefined :: HList l)

-- | Get the union alignment (i.e. the maximum of the types in the union)
unionAlignment :: forall l . HFoldr' Alignment Int l Int => Union l -> Int
unionAlignment _ = hFoldr' Alignment (0 :: Int) (undefined :: HList l)


-------------------------------------------------------------------------------------
-- Finally we can write the Storable and CStorable instances
-------------------------------------------------------------------------------------

instance (HFoldr' SizeOf Int l Int, HFoldr' Alignment Int l Int) => Storable (Union l) where
   sizeOf             = unionSize
   alignment          = unionAlignment
   peek ptr = do
      let sz = sizeOf (undefined :: Union l)
      fp <- mallocForeignPtrBytes sz
      withForeignPtr fp $ \p -> 
         memCopy p (castPtr ptr) (fromIntegral sz)
      return (Union fp)

   poke ptr (Union fp) = do
      let sz = sizeOf (undefined :: Union l)
      withForeignPtr fp $ \p ->
         memCopy (castPtr ptr) p (fromIntegral sz)

instance (Storable (Union l)) => CStorable (Union l) where
   cPeek      = peek
   cPoke      = poke
   cAlignment = alignment
   cSizeOf    = sizeOf


-- TODO: rewrite rules
-- poke p (toUnion x) = poke (castPtr p) x
--
-- (fromUnion <$> peek p) :: IO a  = peek (castPtr p) :: IO a
