{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Pointers
--
-- A pointer is a number: an offset into a memory. This is the `Addr#` type.
--
-- We want the type-system to help us avoid errors when we use pointers, hence
-- we decorate them with phantom types describing the memory layout at the
-- pointed address. This is the `Ptr a` data type that wraps an `Addr#`.
--
-- We often want to associate finalizers to pointers, i.e., actions to be run
-- when the pointer is collected by the GC. These actions take the pointer as a
-- parameter. This is the `ForeignPtr a` data type.
--
-- A `ForeignPtr a` cannot be manipulated like a number because somehow we need
-- to keep the pointer value that will be passed to the finalizers. Moreover we
-- don't want finalizers to be executed too early, so we can't easily create a
-- new ForeignPtr from another (it would require a way to disable the existing
-- finalizers of a ForeignPtr, which would in turn open a whole can of worms).
-- Hence we use the `FinalizedPtr a` pointer type, which has an additional
-- offset field.
module ViperVM.Format.Binary.Ptr
   ( PtrLike (..)
   -- * Pointer
   , Ptr
   , Ptr.free
   -- * Finalized pointer
   , FinalizedPtr (..)
   , withFinalizedPtr
   -- * Foreign pointer
   , ForeignPtr
   , FP.withForeignPtr
   , FP.mallocForeignPtrBytes
   , nullForeignPtr
   -- * Function pointer
   , Ptr.FunPtr
   , Ptr.nullFunPtr
   , Ptr.castPtrToFunPtr
   , Ptr.castFunPtrToPtr
   -- * Pointer as a Word
   , Ptr.WordPtr
   , Ptr.wordPtrToPtr
   , Ptr.ptrToWordPtr
   )
where

import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Marshal.Alloc     as Ptr
import qualified Foreign.ForeignPtr        as FP
import qualified Foreign.ForeignPtr.Unsafe as FP
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr)
import Unsafe.Coerce
import System.IO.Unsafe

import ViperVM.Format.Binary.Layout
import ViperVM.Utils.Types


-- | A finalized pointer
--
-- We use an offset because we can't modify the pointer directly (it is
-- passed to the foreign pointer destructors)
data FinalizedPtr l = FinalizedPtr {-# UNPACK #-} !(ForeignPtr l)
                                   {-# UNPACK #-} !Word  -- offset

type role FinalizedPtr phantom

instance Show (FinalizedPtr l) where
   show (FinalizedPtr fp o) = show (FP.unsafeForeignPtrToPtr fp 
                                    `indexPtr` fromIntegral o)

-- | Null foreign pointer
nullForeignPtr :: ForeignPtr a
{-# NOINLINE nullForeignPtr #-}
nullForeignPtr = unsafePerformIO $ FP.newForeignPtr_ nullPtr

-- | Null finalized pointer
nullFinalizedPtr :: FinalizedPtr a
nullFinalizedPtr = FinalizedPtr nullForeignPtr 0

-- | Use a finalized pointer
withFinalizedPtr :: FinalizedPtr a -> (Ptr a -> IO b) -> IO b
{-# INLINE withFinalizedPtr #-}
withFinalizedPtr (FinalizedPtr fp o) f =
   FP.withForeignPtr fp (f . (`indexPtr` fromIntegral o))

-- | Pointer operations
class PtrLike (p :: * -> *) where
   -- | Cast a pointer from one type to another
   castPtr :: p a -> p b
   {-# INLINE castPtr #-}
   castPtr = unsafeCoerce

   -- | Null pointer (offset is 0)
   nullPtr :: forall a. p a

   -- | Advance a pointer by the given amount of bytes (may be negative)
   indexPtr :: p a -> Int -> p a

   -- | Distance between two pointers in bytes (p2 - p1)
   ptrDistance :: p a -> p b -> Int

   -- | Use the pointer
   withPtr :: p a -> (Ptr a -> IO b) -> IO b

   -- | Malloc the given number of bytes
   mallocBytes :: Word -> IO (p a)

   -- | Add offset to the given layout field
   indexField :: forall path l.
      ( KnownNat (LayoutPathOffset l path)
      ) => p l -> path -> p (LayoutPathType l path)
   {-# INLINE indexField #-}
   indexField p _ = castPtr (p `indexPtr` natValue @(LayoutPathOffset l path))

   -- | Add offset corresponding to the layout field with the given symbol
   (-->) :: forall s l.
      ( KnownNat (LayoutPathOffset l (LayoutPath '[LayoutSymbol s]))
      ) => p l -> LayoutSymbol s -> p (LayoutPathType l (LayoutPath '[LayoutSymbol s]))
   {-# INLINE (-->) #-}
   (-->) l _ = indexField l (layoutSymbol :: LayoutPath '[LayoutSymbol s])

   -- | Add offset corresponding to the layout field with the given index
   (-#>) :: forall n l.
      ( KnownNat (LayoutPathOffset l (LayoutPath '[LayoutIndex n]))
      ) => p l -> LayoutIndex n -> p (LayoutPathType l (LayoutPath '[LayoutIndex n]))
   {-# INLINE (-#>) #-}
   (-#>) l _ = indexField l (layoutIndex :: LayoutPath '[LayoutIndex n])

-- TODO
-- {-# RULES
--  "indexField concat paths" forall l p1 p2 .
--       indexField (indexField l p1) p2 = indexField l (concatPaths p1 p2)
--  #-}
-- concatLayoutPaths :: LayoutPath p1 -> LayoutPath p2 -> LayoutPath (Concat p1 p2)
-- concatPaths = undefined



instance PtrLike Ptr where
   {-# INLINE nullPtr #-}
   nullPtr = Ptr.nullPtr

   {-# INLINE indexPtr #-}
   indexPtr = Ptr.plusPtr

   {-# INLINE ptrDistance #-}
   ptrDistance = Ptr.minusPtr

   {-# INLINE withPtr #-}
   withPtr p f = f p

   {-# INLINE mallocBytes #-}
   mallocBytes = Ptr.mallocBytes . fromIntegral


instance PtrLike FinalizedPtr where
   {-# INLINE nullPtr #-}
   nullPtr = nullFinalizedPtr

   {-# INLINE indexPtr #-}
   indexPtr (FinalizedPtr fp o) n
      | n >= 0    = FinalizedPtr fp (o+fromIntegral n)
      | otherwise = FinalizedPtr fp (o-fromIntegral (abs n))

   {-# INLINE ptrDistance #-}
   ptrDistance (FinalizedPtr fp1 o1) (FinalizedPtr fp2 o2)
      | o2 > o1   = d + fromIntegral (o2 - o1)
      | otherwise = d - fromIntegral (o1 - o2)
      where
         d = ptrDistance (FP.unsafeForeignPtrToPtr fp1)
                         (FP.unsafeForeignPtrToPtr fp2)

   {-# INLINE withPtr #-}
   withPtr = withFinalizedPtr

   {-# INLINE mallocBytes #-}
   mallocBytes n = do
      fp <- FP.mallocForeignPtrBytes (fromIntegral n)
      return (FinalizedPtr fp 0)
