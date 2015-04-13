module ViperVM.Arch.Linux.Input.Sound
   ( Sound(..)
   , getDeviceSoundStatus
   )
where

import qualified Data.ByteString as BS

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

data Sound
   = SoundClick
   | SoundBell
   | SoundTone
   deriving (Eq,Show,Bounded,Enum)


-- | Get sound status (one bit per sound)
--
-- EVIOCGSND
getDeviceSoundStatus :: IOCTL -> FileDescriptor -> SysRet BS.ByteString
getDeviceSoundStatus ioctl fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x1a defaultCheck ((n `div` 8) + 1) fd
   where
      n = fromEnum (maxBound :: Sound) + 1

