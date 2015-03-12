{-# LANGUAGE DeriveGeneric #-}

-- | Graphic card capabilities
module ViperVM.Arch.Linux.Graphics.LowLevel.Capability
   ( Capability(..)
   , cardCapability
   )
where

import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)
import Data.Word

import ViperVM.Arch.Linux.Graphics.LowLevel.Card
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Ioctl

-- | Graphic card capability
data Capability
   = CapGenericBuffer            -- ^ Support generic buffers (i.e. not vendor specific)
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
cardCapability :: Card -> Capability -> SysRet Word64
cardCapability card cap = withCard card $ \ioctl fd -> do
   let param = GetCapability (fromIntegral $ fromEnum cap) 0
   ret <- ioctlReadWrite ioctl 0x64 0x0c defaultCheck fd param
   case ret of
      Left err -> return (Left err)
      Right (GetCapability _ value) -> return (Right value)
