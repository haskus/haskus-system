{-# LANGUAGE DeriveGeneric #-}

-- | Graphic card management
module ViperVM.Arch.Linux.Graphics.LowLevel.Capability
   ( Capability(..)
   , getCapability
   )
where

import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)
import Data.Word

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

data Capability
   = CapGenericBuffer
   | CapVBlankHighController
   | CapGenericPreferredDepth
   | CapGenericPreferShadow
   | CapPrime
   | CapTimestampMonotonic
   | CapAsyncPageFlip
   deriving (Show,Eq)

instance Enum Capability where
   fromEnum x = case x of 
      CapGenericBuffer           -> 1
      CapVBlankHighController -> 2
      CapGenericPreferredDepth   -> 3
      CapGenericPreferShadow     -> 4
      CapPrime                -> 5
      CapTimestampMonotonic   -> 6
      CapAsyncPageFlip        -> 7
   toEnum x = case x of 
      1 -> CapGenericBuffer
      2 -> CapVBlankHighController
      3 -> CapGenericPreferredDepth
      4 -> CapGenericPreferShadow
      5 -> CapPrime
      6 -> CapTimestampMonotonic
      7 -> CapAsyncPageFlip
      _ -> error "Unknown capability"

-- | Parameter for getCapability IOCTL (capability id, return value)
data GetCapability =
   GetCapability Word64 Word64
   deriving (Generic)

instance CStorable GetCapability
instance Storable GetCapability where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Indicate if the given capability is supported
getCapability :: IOCTL -> FileDescriptor -> Capability -> SysRet Word64
getCapability ioctl fd cap = do
   let param = GetCapability (fromIntegral $ fromEnum cap) 0
   ret <- ioctlReadWrite ioctl 0x64 0x0c defaultCheck fd param
   case ret of
      Left err -> return (Left err)
      Right (GetCapability _ value) -> return (Right value)
