{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Storable class
module ViperVM.Format.Binary.Storable
   ( StaticStorable (..)
   , RequiredPadding
   , Padding
   , PaddingEx
   , staticSizeOf
   , staticAlignment
   , module Foreign.Storable
   , module Foreign.CStorable
   , wordBytes
   )
where

import qualified Foreign.Storable as FS
import Foreign.Marshal.Utils
import Foreign.CStorable
import Foreign.Storable
import System.IO.Unsafe

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types

-- | A storable data in constant space whose size is known at compile time
class StaticStorable a where
   -- | Size of the stored data (in bytes)
   type SizeOf a    :: Nat

   -- | Alignment requirement (in bytes)
   type Alignment a :: Nat

   -- | Peek (read) a value from a memory address
   staticPeek :: Ptr a -> IO a

   -- | Poke (write) a value at the given memory address
   staticPoke :: Ptr a -> a -> IO ()


-- | Compute the required padding between a and b to respect b's alignment
type family RequiredPadding a b where
   RequiredPadding a b = Padding (SizeOf a) b

-- | Compute the required padding between the size sz and b to respect b's alignment
type family Padding (sz :: Nat) b where
   Padding sz b = PaddingEx (Modulo sz (Alignment b)) (Alignment b)

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m


-- | Get statically known size
staticSizeOf :: forall a.
   ( StaticStorable a
   , KnownNat (SizeOf a)
   ) => a -> Word
staticSizeOf _ = natValue' @(SizeOf a)

-- | Get statically known alignment
staticAlignment :: forall a.
   ( StaticStorable a
   , KnownNat (Alignment a)
   ) => a -> Word
staticAlignment _ = natValue' @(Alignment a)


instance StaticStorable Word8 where
   type SizeOf    Word8 = 1
   type Alignment Word8 = 1
   staticPeek = FS.peek
   staticPoke = FS.poke

instance StaticStorable Word16 where
   type SizeOf    Word16 = 2
   type Alignment Word16 = 2
   staticPeek = FS.peek
   staticPoke = FS.poke

instance StaticStorable Word32 where
   type SizeOf    Word32 = 4
   type Alignment Word32 = 4
   staticPeek = FS.peek
   staticPoke = FS.poke

instance StaticStorable Word64 where
   type SizeOf    Word64 = 8
   type Alignment Word64 = 8
   staticPeek = FS.peek
   staticPoke = FS.poke

instance StaticStorable Int8 where
   type SizeOf    Int8 = 1
   type Alignment Int8 = 1
   staticPeek = FS.peek
   staticPoke = FS.poke

instance StaticStorable Int16 where
   type SizeOf    Int16 = 2
   type Alignment Int16 = 2
   staticPeek = FS.peek
   staticPoke = FS.poke

instance StaticStorable Int32 where
   type SizeOf    Int32 = 4
   type Alignment Int32 = 4
   staticPeek = FS.peek
   staticPoke = FS.poke

instance StaticStorable Int64 where
   type SizeOf    Int64 = 8
   type Alignment Int64 = 8
   staticPeek = FS.peek
   staticPoke = FS.poke


-- | Get bytes in host-endianness order
wordBytes :: forall a.
   ( Storable a
   , StaticStorable a
   , KnownNat (SizeOf a)
   ) => a -> [Word8]
{-# INLINE wordBytes #-}
wordBytes x = unsafePerformIO $
   with x $ \p -> mapM (peekByteOff p) [0..natValue @(SizeOf a) - 1]

