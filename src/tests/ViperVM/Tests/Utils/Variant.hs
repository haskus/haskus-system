{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module ViperVM.Tests.Utils.Variant
   ( testsVariant
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)
import Data.Either

import ViperVM.Utils.Variant

data A = A deriving (Show,Eq)
data B = B deriving (Show,Eq)
data C = C deriving (Show,Eq)
data D = D deriving (Show,Eq)
data E = E deriving (Show,Eq)
data F = F deriving (Show,Eq)

type ABC = Variant '[A,B,C]
type DEF = Variant '[D,E,F]

b :: ABC
b = setVariantN @1 B

b2d :: B -> D
b2d = const D

c2d :: C -> D
c2d = const D

b2def :: B -> DEF
b2def = const (setVariant E)

c2def :: C -> DEF
c2def = const (setVariant E)


testsVariant :: Test
testsVariant = testGroup "Variant" $
   [ testProperty "set/get by index (match)"
         (getVariantN @1 b == Just B)
   , testProperty "set/get by index (dont' match)"
         (getVariantN @0 b == Nothing)
   , testProperty "set/get by type (match)"
         (getVariant    (setVariant B :: ABC) == Just B)
   , testProperty "set/get by type (don't match)"
         (getVariant @C (setVariant B :: ABC) == Nothing)

   , testProperty "variant equality (match)"
         (b == b)
   , testProperty "variant equality (don't match)"
         (b /= setVariant C)

   , testProperty "update by index (match)"
         (updateVariantN @1 (const D) b == setVariantN @1 D)
   , testProperty "update by index (don't match)"
         (updateVariantN @0 (const F) b == setVariantN @1 B)
   , testProperty "update by type (match)"
         (updateVariant b2d b == setVariantN @1 D)
   , testProperty "update by type (don't match)"
         (updateVariant c2d b == setVariant B)
   , testProperty "update/fold by index (match)"
         (updateVariantFold @1 b2def b == setVariant E)
   , testProperty "update/fold by index (don't match)"
         (updateVariantFold @2 c2def b == setVariant B)

   , testProperty "Convert into tuple"
         (variantToTuple b == (Nothing, Just B, Nothing))
   , testProperty "Convert single variant"
         (singleVariant (setVariant A :: Variant '[A]) == A)

   , testProperty "Lift Either: Left"
         (liftEither (Left A :: Either A B) == setVariant A)
   , testProperty "Lift Either: Right"
         (liftEither (Right B :: Either A B) == setVariant B)

   , testProperty "To Either: Left"
         (toEither (setVariant B :: Variant '[A,B]) == Left B)
   , testProperty "To Either: Right"
         (toEither (setVariant A :: Variant '[A,B]) == Right A)

   , testProperty "headVariant (match)"
         (headVariant (setVariant A :: ABC) == Right A)
   , testProperty "headVariant (don't match)"
         (isLeft (headVariant b))

   , testProperty "pickVariant (match)"
         (pickVariant @1 b == Right B)
   , testProperty "pickVariant (don't match)"
         (isLeft (pickVariant @2 b))

   , testProperty "prependVariant"
         (getVariantN @4 (prependVariant @'[D,E,F] b) == Just B)
   , testProperty "appendVariant"
         (getVariantN @1 (appendVariant @'[D,E,F] b)  == Just B)

   , testProperty "liftVariant"
         (getVariant (liftVariant b :: Variant '[D,A,E,B,F,C])  == Just B)
   ]
