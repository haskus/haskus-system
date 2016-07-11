{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Record (similar to C struct)
module ViperVM.Format.Binary.Record
   ( Record
   , Field
   , RecordSize
   , Alignment
   , Modulo
   , Path
   , recordSize
   , recordAlignment
   , recordField
   , recordFieldOffset
   , recordFieldPath
   , recordFieldPathOffset
   , recordToList
   )
where

import Data.Proxy
import GHC.TypeLits
import Foreign.ForeignPtr
import System.IO.Unsafe

import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.HList
import ViperVM.Utils.Memory
import ViperVM.Utils.Types

-- | Record
newtype Record (fields :: [*]) = Record (ForeignPtr ())

-- | Field
data Field (name :: Symbol) typ

-- | Get record size without the ending padding bytes
type family RecordSize (fs :: [*]) (sz :: Nat) where
   RecordSize '[] sz                    = sz
   RecordSize (Field name typ ': fs) sz = 
      RecordSize fs
         (sz
         -- padding bytes
         + Padding sz typ
         -- field size
         + SizeOf typ
         )

type family FieldOffset (name :: Symbol) (fs :: [*]) (sz :: Nat) where
   -- Found
   FieldOffset name (Field name typ ': fs) sz =
      sz + Padding sz typ
   -- Not found yet
   FieldOffset name (Field xx typ ': fs) sz =
      FieldOffset name fs
         (sz + Padding sz typ + SizeOf typ)

type family FieldType (name :: Symbol) (fs :: [*]) where
   FieldType name (Field name typ ': fs) = typ
   FieldType name (Field xx typ ': fs)   = FieldType name fs

-- | Record size (with ending padding bytes)
type family FullRecordSize fs where
   FullRecordSize fs =
      RecordSize fs 0
      + PaddingEx (Modulo (RecordSize fs 0) (RecordAlignment fs 1))
         (RecordAlignment fs 1)

-- | Record alignment
type family RecordAlignment (fs :: [*]) a where
   RecordAlignment '[]                    a = a
   RecordAlignment (Field name typ ': fs) a =
      RecordAlignment fs
         (IfThenElse (a <=? Alignment typ) (Alignment typ) a)

-- | Return offset from a field path
type family FieldPathOffset (fs :: [*]) (path :: [*]) (off :: Nat) where
   FieldPathOffset fs '[Proxy p] off = off + FieldOffset p fs 0
   FieldPathOffset fs (Proxy p ': ps) off
      = FieldPathOffset (ExtractRecord (FieldType p fs))
            ps (off + FieldOffset p fs 0)

-- | Return type from a field path
type family FieldPathType (fs :: [*]) (path :: [*]) where
   FieldPathType fs '[Proxy p] = FieldType p fs

   FieldPathType fs (Proxy p ': ps)
      = FieldPathType (ExtractRecord (FieldType p fs)) ps
   
type family ExtractRecord x where
   ExtractRecord (Record fs) = fs

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
   ) => Proxy (name :: Symbol) -> Record fs -> Int
recordFieldOffset _ _ = fromIntegral $ natVal (Proxy :: Proxy o)

-- | Get a field
recordField :: forall name a fs o.
   ( o ~ FieldOffset name fs 0
   , a ~ FieldType name fs
   , KnownNat o
   , StaticStorable a
   ) => Proxy (name :: Symbol) -> Record fs -> a
recordField p r@(Record fp) = unsafePerformIO $
   withForeignPtr fp $ \ptr ->do
      let ptr' = ptr `plusPtr` recordFieldOffset p r
      staticPeek (castPtr ptr')

data Path (fs :: [*])

-- | Get a field offset from its path
recordFieldPathOffset :: forall path fs o.
   ( o ~ FieldPathOffset fs path 0
   , KnownNat o
   ) => Path path -> Record fs -> Int
recordFieldPathOffset _ _ = o
   where
      o    = fromIntegral (natVal (Proxy :: Proxy o))

-- | Get a field from its path
recordFieldPath :: forall path a fs o.
   ( o ~ FieldPathOffset fs path 0
   , a ~ FieldPathType fs path
   , KnownNat o
   , StaticStorable a
   ) => Path path -> Record fs -> a
recordFieldPath _ (Record fp) = unsafePerformIO $
   withForeignPtr fp $ \ptr -> do
      let
         o    = fromIntegral (natVal (Proxy :: Proxy o))
         ptr' = ptr `plusPtr` o
      staticPeek (castPtr ptr')


instance forall fs s.
      ( s ~ FullRecordSize fs
      , KnownNat s
      )
      => StaticStorable (Record fs)
   where
      type SizeOf (Record fs)    = FullRecordSize fs
      type Alignment (Record fs) = RecordAlignment fs 1

      staticPeek ptr = do
         let sz = recordSize (undefined :: Record fs)
         fp <- mallocForeignPtrBytes (fromIntegral sz)
         withForeignPtr fp $ \p ->
            memCopy p ptr (fromIntegral sz)
         return (Record fp)

      staticPoke ptr (Record fp) = do
         let sz = recordSize (undefined :: Record fs)
         withForeignPtr fp $ \p ->
            memCopy ptr p (fromIntegral sz)


data Extract = Extract

instance forall fs typ name rec b l2 i r.
   ( rec ~ Record fs                        -- the record
   , b ~ Field name typ                     -- the current field
   , i ~ (rec, HList l2)                    -- input type
   , typ ~ FieldType name fs
   , KnownNat (FieldOffset name fs 0)
   , StaticStorable typ
   , KnownSymbol name
   , r ~ (rec, HList ((String,typ) ': l2))  -- result type
   ) => ApplyAB Extract (b, i) r where
      applyAB _ (_, (rec,xs)) =
         (rec, HCons (symbolVal (Proxy :: Proxy name), recordField (Proxy :: Proxy name) rec) xs)

-- | Convert a record into a HList
recordToList :: forall fs l.
   ( HFoldr' Extract (Record fs, HList '[]) fs (Record fs, HList l)
   ) => Record fs -> HList l
recordToList rec = snd res
   where
      res :: (Record fs, HList l)
      res = hFoldr' Extract ((rec,HNil) :: (Record fs, HList '[])) (undefined :: HList fs)


instance forall fs l.
      ( HFoldr' Extract (Record fs, HList '[]) fs (Record fs, HList l)
      , Show (HList l)
      )
      => Show (Record fs)
   where
      show rec = show (recordToList rec :: HList l)
