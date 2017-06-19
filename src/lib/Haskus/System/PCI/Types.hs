module Haskus.System.PCI.Types
   ( Vendor (..)
   , Device (..)
   , Class (..)
   , SubClass (..)
   )
where

import Data.IntMap.Strict

data Vendor = Vendor
   { vendorName    :: String
   , vendorDevices :: IntMap Device
   } deriving (Show)

data Device = Device
   { deviceName       :: String
   , deviceSubDevices :: IntMap String
   } deriving (Show)

data Class = Class
   { className       :: String
   , classSubClasses :: IntMap SubClass
   } deriving (Show)

data SubClass = SubClass
   { subclassName       :: String
   , subclassInterfaces :: IntMap String
   } deriving (Show)
