{-# LANGUAGE OverloadedStrings #-}

module Haskus.Tests.System.Devices
   ( testsDevices
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic

import Haskus.System.Devices

import Data.Maybe
import qualified Data.Map as Map
import Control.Concurrent.STM


treeRoot :: IO DeviceTree
treeRoot = deviceTreeCreate Nothing Nothing Map.empty

treeXYZ :: IO DeviceTree
treeXYZ = deviceTreeCreate (Just "XYZ") Nothing Map.empty

treeABC :: IO DeviceTree
treeABC = deviceTreeCreate (Just "ABC") Nothing Map.empty

testsDevices :: TestTree
testsDevices = testGroup "Device tree"
   [ testProperty "Insert/lookup" $ monadicIO $ do
         let path = "/devices/xyz"
         tree <- run $ do  
            s <- deviceTreeInsert path <$> treeXYZ <*> treeRoot
            atomically s
         let xyz = deviceTreeLookup path tree
         assert (isJust xyz)
         assert (deviceNodeSubsystem (fromJust xyz) == Just "XYZ")

   , testProperty "Insert/remove" $ monadicIO $ do
         let path = "/devices/xyz"
         tree <- run $ do  
            s <- deviceTreeInsert path <$> treeXYZ <*> treeRoot
            atomically s
         let xyz = deviceTreeLookup path (deviceTreeRemove path tree)
         assert (isNothing xyz)

   , testProperty "Insert/lookup hierarchy" $ monadicIO $ do
         let path0 = "/devices/xyz"
         let path1 = "/devices/xyz/abc"
         tree <- run $ do
            xyz  <- treeXYZ
            abc  <- treeABC
            root <- treeRoot
            atomically $ do
               t1 <- deviceTreeInsert path0 xyz root
               deviceTreeInsert path1 abc t1
         let abc = deviceTreeLookup path1 tree
         assert (isJust abc)
         assert (deviceNodeSubsystem (fromJust abc) == Just "ABC")

   ]
