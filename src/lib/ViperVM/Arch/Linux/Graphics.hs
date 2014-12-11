-- | Interface to Linux graphics API
module ViperVM.Arch.Linux.Graphics
   ( Capability(..)
   , drmIoctl
   , getCapability
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Foreign.Ptr
import Data.Word

-- Linux currently uses KMS/DRM interface


-------------------------------------------
-- From drm/xf86drm.h
-------------------------------------------

-- | IOCTL for DRM is restarted on interruption
-- Apply this function to your preferred ioctl function
drmIoctl :: IOCTL -> IOCTL
drmIoctl = repeatIoctl

data Capability
   = CapDUMMY     -- Added to easily derive Enum (start at 0x01...)
   | CapDumbBuffer
   | CapVBlankHighCRTC
   | CapDumbPreferredDepth
   | CapDumbPreferShadow
   | CapPrime
   | CapTimestampMonotonic
   | CapAsyncPageFlip
   deriving (Show,Eq,Enum)

-- | Parameter for getCapability IOCTL (capability id, return value)
data GetCapability = GetCapability Word64 Word64

instance Storable GetCapability where
   sizeOf _ = 16
   alignment _ = 8
   peek ptr = 
      let p = castPtr ptr :: Ptr Word64 in
         GetCapability <$> peekElemOff p 0 <*> peekElemOff p 1
   poke ptr (GetCapability x y) = do
      let p = castPtr ptr :: Ptr Word64
      pokeElemOff p 0 x
      pokeElemOff p 1 y

-- | Indicate if the given capability is supported
getCapability :: IOCTL -> FileDescriptor -> Capability -> SysRet Word64
getCapability ioctl fd cap = do
   let param = GetCapability (fromIntegral $ fromEnum cap) 0
   ret <- ioctlReadWrite ioctl 0x64 0x0c defaultCheck fd param
   case ret of
      Left err -> return (Left err)
      Right (GetCapability _ value) -> return (Right value)

