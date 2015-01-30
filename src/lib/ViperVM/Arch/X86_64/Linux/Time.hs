{-# LANGUAGE DeriveGeneric #-}
module ViperVM.Arch.X86_64.Linux.Time (
   TimeSpec(..)
) where

import Foreign.Storable
import Foreign.CStorable
import Data.Int

import GHC.Generics (Generic)

data TimeSpec = TimeSpec {
   tsSeconds      :: Int64,
   tsNanosecondes :: Int64
} deriving (Show,Eq,Generic)

instance CStorable TimeSpec
instance Storable TimeSpec where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek
