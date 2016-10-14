{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generics
module ViperVM.Utils.Types.Generics
   ( module GHC.Generics
   -- * Fields
   , Field
   , FieldType
   , LookupField
   , LookupFieldType
   -- * Data type fields
   , ExtractFields
   , ExtractFieldTypes
   )
where

import ViperVM.Utils.Types.List
import ViperVM.Utils.Types
import GHC.Generics

-- | Named field
data Field (name :: Symbol) (t :: *)

type family FieldType f where
   FieldType (Field name t) = t

type family LookupFieldType fs s where
   LookupFieldType fs s = FieldType (LookupField fs s)

type family LookupField (fs :: [*]) (s :: Symbol) where
   LookupField (Field name t ': fs) name = Field name t
   LookupField (Field name t ': fs) s    = LookupField fs s
   LookupField '[]                  name =
      TypeError ('Text "Cannot find field with name: " ':<>: 'ShowType name)


-- | Extract fields of a data type:
--    - require selector symbols
--    - only support data type with a single constructor
type family ExtractFields (a :: *)  where
   ExtractFields a = ExtractFields' (Rep a)

type family ExtractFields' a where
   -- extract constructors
   ExtractFields' (D1 _ cs)   = ExtractFields' cs

   -- extract selectors
   ExtractFields' (C1 _ ss)   = ExtractFields' ss
   ExtractFields' (s1 :*: s2) = Concat (ExtractFields' s1) (ExtractFields' s2)

   -- extract field name and type from the selector
   ExtractFields' (S1 ('MetaSel ('Just name) _ _ _) (Rec0 t)) = '[Field name t]



-- | Extract types of the fields of a data type
--    - only support data type with a single constructor
type family ExtractFieldTypes (a :: *)  where
   ExtractFieldTypes a = ExtractFieldTypes' (Rep a)

type family ExtractFieldTypes' a where
   -- extract constructors
   ExtractFieldTypes' (D1 _ cs)   = ExtractFieldTypes' cs

   -- extract selectors
   ExtractFieldTypes' (C1 _ ss)   = ExtractFieldTypes' ss
   ExtractFieldTypes' (s1 :*: s2) =
      Concat (ExtractFieldTypes' s1) (ExtractFieldTypes' s2)

   -- extract field type from the selector
   ExtractFieldTypes' (S1 _ (Rec0 t)) = '[t]
