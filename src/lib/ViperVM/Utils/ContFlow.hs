{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}

-- | Continuation based control-flow
module ViperVM.Utils.ContFlow
   ( ContFlow (..)
   , (>::>)
   , (>:-:>)
   , (>:~:>)
   , fret
   , fretN
   , freturn
   , freturnN
   , frec
   , ContListToTuple
   , ContTupleToList
   , StripR
   , AddR
   )
where

import ViperVM.Utils.Tuple
import ViperVM.Utils.Types
import ViperVM.Utils.Types.List

-- | A continuation based control-flow
newtype ContFlow (xs :: [*]) r = ContFlow (ContListToTuple xs r -> r)

-- | Convert a list of types into the actual data type representing the
-- continuations.
type family ContListToTuple (xs :: [*]) r where
   ContListToTuple xs r = ListToTuple (AddR xs r)

-- | Convert a tuple of continuations into a list of types
type family ContTupleToList t r :: [*] where
   ContTupleToList t r = StripR (TupleToList t) r

type family AddR f r where
   AddR '[] r       = '[]
   AddR (x ': xs) r = (x -> r) ': AddR xs r

type family StripR f r where
   StripR '[] r              = '[]
   StripR ((x -> r) ': xs) r = x ': StripR xs r
   StripR ((x -> w) ': xs) r =
      TypeError ( 'Text "Invalid continuation return type `"
                  ':<>: 'ShowType w ':<>: 'Text "', expecting `"
                  ':<>: 'ShowType r ':<>: 'Text "'")

-- | Bind a flow to a tuple of continuations
(>::>) :: ContFlow xs r -> ContListToTuple xs r -> r
{-# INLINE (>::>) #-}
(>::>) (ContFlow f) !cs = f cs

-- | Bind a flow to a 1-tuple of continuations
(>:-:>) :: ContFlow '[a] r -> (a -> r) -> r
{-# INLINE (>:-:>) #-}
(>:-:>) (ContFlow f) c = f (Single c)


-- | Bind a flow to a tuple of continuations and
-- reorder fields if necessary
(>:~:>) :: forall ts xs r.
   ( ReorderTuple ts (ContListToTuple xs r)
   ) => ContFlow xs r -> ts -> r
{-# INLINE (>:~:>) #-}
(>:~:>) (ContFlow f) !cs = f (tupleReorder cs)

-- | Call the type-indexed continuation from the tuple passed as first parameter
fret :: forall x r t n xs.
   ( ExtractTuple n t (x -> r)
   , xs ~ ContTupleToList t r
   , Member x xs
   , n ~ IndexOf x xs
   , KnownNat n
   , CheckNub xs
   ) => t -> (x -> r)
{-# INLINE fret #-}
fret = tupleN @n @t @(x -> r)

-- | Implicitly call the type-indexed continuation in the context
freturn :: forall x r t n xs.
   ( ExtractTuple n t (x -> r)
   , xs ~ ContTupleToList t r
   , Member x xs
   , n ~ IndexOf x xs
   , KnownNat n
   , CheckNub xs
   , ?__cs :: t
   ) => x -> r
{-# INLINE freturn #-}
freturn = fret ?__cs

-- | Call the indexed continuation from the tuple passed as first parameter
fretN :: forall n x r t xs.
   ( ExtractTuple n t (x -> r)
   , xs ~ ContTupleToList t r
   , x ~ TypeAt n xs
   , KnownNat n
   ) => t -> (x -> r)
{-# INLINE fretN #-}
fretN = tupleN @n @t @(x -> r)


-- | Implicitly call the type-indexed continuation in the context
freturnN :: forall n x r t xs.
   ( ExtractTuple n t (x -> r)
   , xs ~ ContTupleToList t r
   , x ~ TypeAt n xs
   , KnownNat n
   , ?__cs :: t
   ) => x -> r
{-# INLINE freturnN #-}
freturnN = fretN @n ?__cs


-- | Recursive call
frec :: forall r xs.
   ( ?__cs :: ContListToTuple xs r
   ) => ContFlow xs r -> r
frec f = f >::> ?__cs

-- this define has to be defined in each module using ContFlow for now
#define fdo ContFlow $ \__cs -> let ?__cs = __cs in do
