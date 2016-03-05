{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Graphic card capabilities
module ViperVM.Arch.Linux.Graphics.Capability
   ( Capability(..)
   , cardCapability
   , cardHasSupportFor
   )
where

import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)
import Data.Word

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Arch.Linux.ErrorCode

-- | Graphic card capability
data Capability
   = CapGenericBuffer         -- ^ Support generic buffers (i.e. not vendor specific)
   | CapVBlankHighController
   | CapGenericPreferredDepth
   | CapGenericPreferShadow
   | CapPrime
   | CapTimestampMonotonic
   | CapAsyncPageFlip         -- ^ Support asynchronous page-flipping
   | CapCursorWidth
   | CapCursorHeight
   | CapAddFrameBufferModifiers
   deriving (Show,Eq,Enum)

-- | Parameter for getCapability IOCTL (capability id, return value)
data GetCapability
   = GetCapability Word64 Word64
   deriving (Generic,CStorable)

instance Storable GetCapability where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Indicate if the given capability is supported
cardCapability :: Card -> Capability -> SysRet Word64
cardCapability card cap = do
   let param = GetCapability (fromIntegral (fromEnum cap + 1)) 0
   ret <- ioctlGetCapabilities (cardHandle card) param
   case ret of
      Left err -> return (Left err)
      Right (GetCapability _ value) -> return (Right value)

-- | Indicate if a capability is supported
cardHasSupportFor :: Card -> Capability -> SysRet Bool
cardHasSupportFor card cap = fmap (/= 0) <$> cardCapability card cap

