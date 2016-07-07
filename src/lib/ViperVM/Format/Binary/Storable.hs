{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Storable class
module ViperVM.Format.Binary.Storable
   ( Storable (..)
   )
where

import GHC.TypeLits
import Foreign.Ptr
import qualified Foreign.Storable as FS

import ViperVM.Format.Binary.Word

-- | A storable data (in constant space)
class Storable a where
   -- | Size of the stored data (in bytes)
   type SizeOf a    :: Nat

   -- | Alignment requirement (in bytes)
   type Alignment a :: Nat

   -- | Peek (read) a value from a memory address
   peek :: Ptr a -> IO a

   -- | Poke (write) a value at the given memory address
   poke :: Ptr a -> a -> IO ()


instance Storable Word8 where
   type SizeOf    Word8 = 1
   type Alignment Word8 = 1
   peek = FS.peek
   poke = FS.poke

instance Storable Word16 where
   type SizeOf    Word16 = 2
   type Alignment Word16 = 2
   peek = FS.peek
   poke = FS.poke

instance Storable Word32 where
   type SizeOf    Word32 = 4
   type Alignment Word32 = 4
   peek = FS.peek
   poke = FS.poke

instance Storable Word64 where
   type SizeOf    Word64 = 8
   type Alignment Word64 = 8
   peek = FS.peek
   poke = FS.poke

instance Storable Int8 where
   type SizeOf    Int8 = 1
   type Alignment Int8 = 1
   peek = FS.peek
   poke = FS.poke

instance Storable Int16 where
   type SizeOf    Int16 = 2
   type Alignment Int16 = 2
   peek = FS.peek
   poke = FS.poke

instance Storable Int32 where
   type SizeOf    Int32 = 4
   type Alignment Int32 = 4
   peek = FS.peek
   poke = FS.poke

instance Storable Int64 where
   type SizeOf    Int64 = 8
   type Alignment Int64 = 8
   peek = FS.peek
   poke = FS.poke
