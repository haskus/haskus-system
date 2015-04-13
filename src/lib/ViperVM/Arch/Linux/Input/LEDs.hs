module ViperVM.Arch.Linux.Input.LEDs
   ( LED(..)
   , getDeviceLEDs
   )
where

import qualified Data.ByteString as BS

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

data LED
   = LEDNumLock
   | LEDCapsLock
   | LEDScrollLock
   | LEDCompose
   | LEDKana
   | LEDSleep
   | LEDSuspend
   | LEDMute
   | LEDMisc
   | LEDMail
   | LEDCharging
   deriving (Eq,Show,Enum,Bounded)

-- | Get leds (one bit per led)
--
-- EVIOCGLED
getDeviceLEDs :: IOCTL -> FileDescriptor -> SysRet BS.ByteString
getDeviceLEDs ioctl fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x19 defaultCheck ((n `div` 8) + 1) fd
   where
      n = fromEnum (maxBound :: LED) + 1
