{-# LANGUAGE TypeFamilies #-}

module ViperVM.Format.Binary.Common
   ( Cons
   , Nil
   , Tail
   , Head
   )
where

-- | Type list
data Cons x xs
data Nil

type family Head a :: * where
   Head (Cons x xs) = x

type family Tail a :: * where
   Tail (Cons x xs) = xs
