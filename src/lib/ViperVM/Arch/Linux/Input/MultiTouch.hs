module ViperVM.Arch.Linux.Input.MultiTouch
   ( MultiTouchToolType(..)
   , getDeviceMultiTouchSlots
   )
where

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl

data MultiTouchToolType
   = MultiTouchFinder
   | MultiTouchPen
   deriving (Show,Eq,Enum)

-- | Get multi-touch slots
--
-- EVIOCGMTSLOTS
getDeviceMultiTouchSlots :: IOCTL -> Word32 -> Int -> FileDescriptor -> SysRet [Int32]
getDeviceMultiTouchSlots ioctl code nSlots fd = do
   let sz = 4 * (nSlots + 1)
   allocaBytes (fromIntegral sz) $ \ptr -> do
      pokeByteOff ptr 0 code
      ret <- ioctlReadBytes ioctl 0x45 0x0a defaultCheck (fromIntegral sz) ptr fd
      case ret of
         Left err -> return (Left err)
         Right _  -> Right <$> peekArray nSlots ptr


