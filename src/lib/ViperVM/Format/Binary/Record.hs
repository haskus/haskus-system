{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
         (IfNat (a <=? Alignment typ) (Alignment typ) a)

-- | Return offset from a field path
type family FieldPathOffset (fs :: [*]) (path :: [Symbol]) (off :: Nat) where
   FieldPathOffset fs '[p] off = off + FieldOffset p fs 0
   FieldPathOffset fs (p ': ps) off
      = FieldPathOffset (ExtractRecord (FieldType p fs))
            ps (off + FieldOffset p fs 0)

-- | Return type from a field path
type family FieldPathType (fs :: [*]) (path :: [Symbol]) where
   FieldPathType fs '[p] = FieldType p fs

   FieldPathType fs (p ': ps)
      = FieldPathType (ExtractRecord (FieldType p fs)) ps
   
type family ExtractRecord x where
   ExtractRecord (Record fs) = fs

-- | Get record size
recordSize :: forall fs.
   ( KnownNat (FullRecordSize fs)
   ) => Record fs -> Word
recordSize _ = natValue' @(FullRecordSize fs)

-- | Get record alignment
recordAlignment :: forall fs.
   ( KnownNat (RecordAlignment fs 1)
   ) => Record fs -> Word
recordAlignment _ = natValue' @(RecordAlignment fs 1)

-- | Get a field offset
recordFieldOffset :: forall (name :: Symbol) fs.
   ( KnownNat (FieldOffset name fs 0)
   ) => Record fs -> Int
recordFieldOffset _ = natValue @(FieldOffset name fs 0)

-- | Get a field
recordField :: forall (name :: Symbol) a fs.
   ( KnownNat (FieldOffset name fs 0)
   , a ~ FieldType name fs
   , StaticStorable a
   ) => Record fs -> a
recordField r@(Record fp) = unsafePerformIO $
   withForeignPtr fp $ \ptr ->do
      let ptr' = ptr `indexPtr` recordFieldOffset @name r
      staticPeek (castPtr ptr')

data Path (fs :: [Symbol])

-- | Get a field offset from its path
recordFieldPathOffset :: forall path fs o.
   ( o ~ FieldPathOffset fs path 0
   , KnownNat o
   ) => Path path -> Record fs -> Int
recordFieldPathOffset _ _ = natValue @o

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
         ptr' = ptr `indexPtr` natValue @o
      staticPeek (castPtr ptr')


instance forall fs s.
      ( s ~ FullRecordSize fs
      , KnownNat s
      )
      => StaticStorable (Record fs)
   where
      type SizeOf (Record fs)    = FullRecordSize fs
      type Alignment (Record fs) = RecordAlignment fs 1

      staticPeekIO ptr = do
         let sz = recordSize (undefined :: Record fs)
         fp <- mallocForeignPtrBytes sz
         withForeignPtr fp $ \p ->
            memCopy p ptr (fromIntegral sz)
         return (Record fp)

      staticPokeIO ptr (Record fp) = do
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
   ) => Apply Extract (b, i) r where
      apply _ (_, (rec,xs)) =
         (rec, HCons (symbolValue @name, recordField @name rec) xs)

-- | Convert a record into a HList
recordToList :: forall fs.
   ( HFoldr' Extract (Record fs, HList '[]) fs (Record fs, HList fs)
   ) => Record fs -> HList fs
recordToList rec = snd res
   where
      res :: (Record fs, HList fs)
      res = hFoldr' Extract ((rec,HNil) :: (Record fs, HList '[])) (undefined :: HList fs)


instance forall fs.
      ( HFoldr' Extract (Record fs, HList '[]) fs (Record fs, HList fs)
      , Show (HList fs)
      )
      => Show (Record fs)
   where
      show rec = show (recordToList rec :: HList fs)
