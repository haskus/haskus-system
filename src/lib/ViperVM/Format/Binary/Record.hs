{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Record (similar to C struct)
module ViperVM.Format.Binary.Record
   ( Record
   , Field
   , RecordSize
   , Alignment
   , Modulo
   , IfThenElse
   , recordSize
   , recordAlignment
   , recordFieldOffset
   , recordField
   )
where

import Data.Proxy
import GHC.TypeLits
import Foreign.ForeignPtr
import Foreign.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.Memory
import System.IO.Unsafe

-- | Record
newtype Record (fields :: [*]) = Record (ForeignPtr ())

-- | Field
data Field (name :: Symbol) typ

type family IfThenElse c (t :: Nat) (e :: Nat) where
   IfThenElse 'True  t e = t
   IfThenElse 'False t e = e

type family Modulo (a :: Nat) (b :: Nat) where
   Modulo a b = ModuloEx (a <=? b) a b

type family ModuloEx c a b where
   ModuloEx 'True  a b = a
   ModuloEx 'False a b = ModuloEx ((a-b) <=? b) (a-b) b

type family Padding modulo alignment where
   Padding 0 a = 0
   Padding m a = a - m

-- | Get record size without the ending padding bytes
type family RecordSize (fs :: [*]) (sz :: Nat) where
   RecordSize '[] sz                    = sz
   RecordSize (Field name typ ': fs) sz = 
      RecordSize fs
         (sz
         -- padding bytes
         + Padding (Modulo sz (Alignment typ)) (Alignment typ)
         -- field size
         + SizeOf typ
         )

type family FieldOffset (name :: Symbol) (fs :: [*]) (sz :: Nat) where
   -- Found
   FieldOffset name (Field name typ ': fs) sz =
      sz + Padding (Modulo sz (Alignment typ)) (Alignment typ)
   -- Not found yet
   FieldOffset name (Field xx typ ': fs) sz =
      FieldOffset name fs
         (sz + Padding (Modulo sz (Alignment typ)) (Alignment typ)
             + SizeOf typ)

type family FieldType (name :: Symbol) (fs :: [*]) where
   FieldType name (Field name typ ': fs) = typ
   FieldType name (Field xx typ ': fs)   = FieldType name fs

-- | Record size (with ending padding bytes)
type family FullRecordSize fs where
   FullRecordSize fs =
      RecordSize fs 0
      + Padding (Modulo (RecordSize fs 0) (RecordAlignment fs 1))
         (RecordAlignment fs 1)

-- | Record alignment
type family RecordAlignment (fs :: [*]) a where
   RecordAlignment '[]                    a = a
   RecordAlignment (Field name typ ': fs) a =
      RecordAlignment fs
         (IfThenElse (a <=? Alignment typ) (Alignment typ) a)

-- | Get record size
recordSize :: forall s fs.
   ( s ~ FullRecordSize fs
   , KnownNat s) => Record fs -> Word
recordSize _ = fromIntegral $ natVal (Proxy :: Proxy s)

-- | Get record alignment
recordAlignment :: forall a fs.
   ( a ~ RecordAlignment fs 1
   , KnownNat a) => Record fs -> Word
recordAlignment _ = fromIntegral $ natVal (Proxy :: Proxy a)

-- | Get a field offset
recordFieldOffset :: forall name fs o.
   ( o ~ FieldOffset name fs 0
   , KnownNat o
   ) => Record fs -> Proxy (name :: Symbol) -> Int
recordFieldOffset _ _ = fromIntegral $ natVal (Proxy :: Proxy o)

-- | Get a field
recordField :: forall name a fs o.
   ( o ~ FieldOffset name fs 0
   , a ~ FieldType name fs
   , KnownNat o
   , Storable a
   ) => Record fs -> Proxy (name :: Symbol) -> a
recordField r@(Record fp) p = unsafePerformIO $
   withForeignPtr fp $ \ptr ->do
      let ptr' = ptr `plusPtr` recordFieldOffset r p
      peek (castPtr ptr')


instance forall fs s.
      ( s ~ FullRecordSize fs
      , KnownNat s
      )
      => Storable (Record fs)
   where
      type SizeOf (Record fs)    = FullRecordSize fs
      type Alignment (Record fs) = RecordAlignment fs 1

      peek ptr = do
         let sz = recordSize (undefined :: Record fs)
         fp <- mallocForeignPtrBytes (fromIntegral sz)
         withForeignPtr fp $ \p ->
            memCopy p ptr (fromIntegral sz)
         return (Record fp)

      poke ptr (Record fp) = do
         let sz = recordSize (undefined :: Record fs)
         withForeignPtr fp $ \p ->
            memCopy ptr p (fromIntegral sz)
