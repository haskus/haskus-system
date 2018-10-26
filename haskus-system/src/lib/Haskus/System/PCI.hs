{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | PCI devices
module Haskus.System.PCI
   ( pciDevices
   , pciClasses
   , lookupVendor
   , lookupDevice
   , lookupSubDevice
   , Vendor (..)
   , Device (..)
   , Class (..)
   , SubClass (..)
   )
where

import Prelude hiding (lookup)
import Data.IntMap.Strict

import Haskus.System.PCI.MakeTable
import Haskus.System.PCI.Types
import Haskus.Format.Binary.Bits

-- | List of PCI vendor/device names
[pcis|src/lib/Haskus/System/PCI/pci.ids|]

-- | Lookup vendor by ID
lookupVendor :: Int -> Maybe Vendor
lookupVendor n = lookup n pciDevices

-- | Lookup device by ID
lookupDevice :: Vendor -> Int -> Maybe Device
lookupDevice v n = lookup n (vendorDevices v)

-- | Lookup subdevice by ID
lookupSubDevice :: Device -> Int -> Int -> Maybe String
lookupSubDevice d vendor dev = lookup n (deviceSubDevices d)
   where n = (vendor `shiftL` 16) .|. dev
