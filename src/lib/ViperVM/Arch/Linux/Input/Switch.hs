module ViperVM.Arch.Linux.Input.Switch
   ( SwitchEvent(..)
   , getDeviceSwitchStatus
   )
where

import qualified Data.ByteString as BS

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

data SwitchEvent
   = SwitchLID                -- ^ set = LID shut
   | SwitchTabletMode         -- ^ set = tablet mode
   | SwitchHeadphoneInsert    -- ^ set = inserted
   | SwitchRFKillAll          -- ^ rfkill master switch, type "any" set = radio enabled
   | SwitchMicroPhoneInsert   -- ^ set = inserted
   | SwitchDock               -- ^ set = plugged into dock
   | SwitchLineOutInsert      -- ^ set = inserted
   | SwitchJackPhysicalInsert -- ^ set = mechanical switch set
   | SwitchVideoOutInsert     -- ^ set = inserted
   | SwitchCameraLensCover    -- ^ set = lens covered
   | SwitchKeypadSlide        -- ^ set = keypad slide out
   | SwitchFrontProximity     -- ^ set = front proximity sensor active
   | SwitchRotateLock         -- ^ set = rotate locked/disabled
   | SwitchLineInInsert       -- ^ set = inserted
   | SwitchMuteDevice         -- ^ set = device disabled
   deriving (Show,Eq,Enum)


-- | Get switch status (one bit per switch)
--
-- EVIOCGSW
getDeviceSwitchStatus :: IOCTL -> Int -> FileDescriptor -> SysRet BS.ByteString
getDeviceSwitchStatus ioctl n fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x1b defaultCheck ((n `div` 8) + 1) fd

