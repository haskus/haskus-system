{-# LANGUAGE DeriveGeneric #-}

module ViperVM.Arch.Linux.Input.Repeat
   ( RepeatSettings(..)
   , AutoRepeat(..)
   , getRepeatSettings
   , setRepeatSettings
   )
where

import Data.Int
import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

data RepeatSettings = RepeatSettings
   { repeatDelay  :: Int32
   , repeatPeriod :: Int32
   }
   deriving (Show,Eq,Generic)

instance CStorable RepeatSettings
instance Storable RepeatSettings where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

data AutoRepeat
   = RepeatDelay
   | RepeatPeriod
   deriving (Eq,Show,Enum)


-- | Get repeat settings
--
-- EVIOCGREP
getRepeatSettings :: IOCTL -> FileDescriptor -> SysRet RepeatSettings
getRepeatSettings ioctl = ioctlRead ioctl 0x45 0x03 defaultCheck

-- | Set repeat settings
--
-- EVIOCSREP
setRepeatSettings :: IOCTL -> FileDescriptor -> RepeatSettings -> SysRet ()
setRepeatSettings ioctl = ioctlWrite ioctl 0x45 0x03 defaultCheckRet


