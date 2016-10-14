{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
   , unionCast
   )
where

import ViperVM.Utils.Memory (memCopy, memSet)
import ViperVM.Utils.Types
import ViperVM.Utils.Types.List hiding (Union)
import ViperVM.Utils.HList
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Ptr

import Foreign.Storable
import Foreign.CStorable
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)


-- TODO: rewrite rules
-- poke p (toUnion x) = poke (castPtr p) x
--
-- (fromUnion <$> peek p) :: IO a  = peek (castPtr p) :: IO a



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
         memSet (p `indexPtr` psz) (fromIntegral (sz - psz)) 0
      poke (castPtr p) v
   return $ Union fp

-- | Convert two storable values
unionCast :: forall a b.
   ( Storable a
   , Storable b
   , Member a '[a,b]
   , Member b '[a,b]
   ) => a -> b
unionCast a = fromUnion u
   where
      u :: Union '[a,b]
      u = toUnion a

type family MapSizeOf fs where
   MapSizeOf '[]       = '[]
   MapSizeOf (x ': xs) = SizeOf x ': MapSizeOf xs

type family MapAlignment fs where
   MapAlignment '[]       = '[]
   MapAlignment (x ': xs) = Alignment x ': MapAlignment xs


instance forall fs.
      ( KnownNat (Max (MapSizeOf fs))
      , KnownNat (Max (MapAlignment fs))
      )
      => StaticStorable (Union fs)
   where
      type SizeOf (Union fs)    = Max (MapSizeOf fs)
      type Alignment (Union fs) = Max (MapAlignment fs)

      staticPeek ptr = do
         let sz = natValue @(SizeOf (Union fs))
         fp <- mallocForeignPtrBytes sz
         withForeignPtr fp $ \p -> 
            memCopy p (castPtr ptr) (fromIntegral sz)
         return (Union fp)

      staticPoke ptr (Union fp) = do
         withForeignPtr fp $ \p ->
            memCopy (castPtr ptr) p (natValue @(SizeOf (Union fs)))

-------------------------------------------------------------------------------------
-- We use HFoldr' to get the maximum size and alignment of the types in the union
-------------------------------------------------------------------------------------

data FoldSizeOf    = FoldSizeOf
data FoldAlignment = FoldAlignment

instance (r ~ Int, Storable a) => Apply FoldSizeOf (a, Int) r where
   apply _ (_,r) = max r (sizeOf (undefined :: a))

instance (r ~ Int, Storable a) => Apply FoldAlignment (a, Int) r where
   apply _ (_,r) = max r (alignment (undefined :: a))

-- | Get the union size (i.e. the maximum of the types in the union)
unionSize :: forall l . HFoldr' FoldSizeOf Int l Int => Union l -> Int
unionSize _ = hFoldr' FoldSizeOf (0 :: Int) (undefined :: HList l)

-- | Get the union alignment (i.e. the maximum of the types in the union)
unionAlignment :: forall l . HFoldr' FoldAlignment Int l Int => Union l -> Int
unionAlignment _ = hFoldr' FoldAlignment (0 :: Int) (undefined :: HList l)


-------------------------------------------------------------------------------------
-- Finally we can write the Storable and CStorable instances
-------------------------------------------------------------------------------------

instance
   ( HFoldr' FoldSizeOf Int l Int
   , HFoldr' FoldAlignment Int l Int
   ) => Storable (Union l) where
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
