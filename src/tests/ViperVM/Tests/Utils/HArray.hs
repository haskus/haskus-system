{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module ViperVM.Tests.Utils.HArray
   ( testsHArray
   )
where

import Distribution.TestSuite (Test,testGroup)
import Distribution.TestSuite.QuickCheck (testProperty)

import ViperVM.Utils.HArray

testsHArray :: Test
testsHArray = testGroup "HArray" $
   [ testGroup "Getters"
      [ testProperty "Get by index 0" (getHArrayN @0 arr1 == 10)
      , testProperty "Get by index 1" (getHArrayN @1 arr1 == "Hello")
      , testProperty "Get by index 2" (getHArrayN @2 arr1 == False)
      , testProperty "Get by type 0"  (getHArrayT arr1 == (10 :: Int))
      , testProperty "Get by type 1"  (getHArrayT arr1 == "Hello")
      , testProperty "Get by type 2"  (getHArrayT arr1 == False)
      ]
   , testGroup "Setters"
      [ testProperty "Set by index 0" (testSetGetN @0 20      arr1)
      , testProperty "Set by index 1" (testSetGetN @1 "World" arr1)
      , testProperty "Set by index 2" (testSetGetN @2 True    arr1)
      , testProperty "Set by type 0"  (testSetGetT (20 :: Int) arr1)
      , testProperty "Set by type 1"  (testSetGetT "World"     arr1)
      , testProperty "Set by type 2"  (testSetGetT True        arr1)
      ]
   , testGroup "Concat"
      [ testProperty "Get by type 0"  (getHArrayT arr12 == (10 :: Int))
      , testProperty "Get by type 1"  (getHArrayT arr12 == "Hello")
      , testProperty "Get by type 2"  (getHArrayT arr12 == False)
      , testProperty "Get by type 3"  (getHArrayT arr12 == (2.5 :: Double))
      , testProperty "Get by type 4"  (getHArrayT arr12 == 'E')
      , testProperty "Get by type 0"  (getHArrayT arr21 == (10 :: Int))
      , testProperty "Get by type 1"  (getHArrayT arr21 == "Hello")
      , testProperty "Get by type 2"  (getHArrayT arr21 == False)
      , testProperty "Get by type 3"  (getHArrayT arr21 == (2.5 :: Double))
      , testProperty "Get by type 4"  (getHArrayT arr21 == 'E')
      , testProperty "Get by index 0" (getHArrayN @0 arr12 == 10)
      , testProperty "Get by index 1" (getHArrayN @1 arr12 == "Hello")
      , testProperty "Get by index 2" (getHArrayN @2 arr12 == False)
      , testProperty "Get by index 3" (getHArrayN @3 arr12 == 2.5)
      , testProperty "Get by index 4" (getHArrayN @4 arr12 == 'E')
      , testProperty "Get by index 0" (getHArrayN @0 arr21 == 2.5)
      , testProperty "Get by index 1" (getHArrayN @1 arr21 == 'E')
      , testProperty "Get by index 2" (getHArrayN @2 arr21 == 10)
      , testProperty "Get by index 3" (getHArrayN @3 arr21 == "Hello")
      , testProperty "Get by index 4" (getHArrayN @4 arr21 == False)
      ]
   , testGroup "TryGetHArray"
      [ testProperty "Try get by type valid"   (tryGetHArrayT arr1 == Just "Hello")
      , testProperty "Try get by type invalid" (tryGetHArrayT arr1 == (Nothing :: Maybe Char))
      ]
   ]


arr1 :: HArray '[Int,String,Bool]
arr1 = prependHArray 10
   $ prependHArray "Hello"
   $ prependHArray False
   $ emptyHArray

testSetGetN :: forall n t ts.
   ( Eq t
   , HArrayIndex n t ts
   ) => t -> HArray ts -> Bool
testSetGetN a as = getHArrayN @n (setHArrayN @n a as) == a

testSetGetT ::
   ( Eq t
   , HArrayIndexT t ts
   ) => t -> HArray ts -> Bool
testSetGetT a as = getHArrayT (setHArrayT a as) == a

arr2 :: HArray '[Double,Char]
arr2 = prependHArray 2.5
   $ prependHArray 'E'
   $ emptyHArray

arr12 :: HArray '[Int,String,Bool,Double,Char]
arr12 = concatHArray arr1 arr2

arr21 :: HArray '[Double,Char,Int,String,Bool]
arr21 = concatHArray arr2 arr1
