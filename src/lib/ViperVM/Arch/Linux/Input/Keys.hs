{-# LANGUAGE DeriveGeneric #-}

-- We need this one to use type literal numbers (S (S .. Z)) of size 32
{-# OPTIONS -fcontext-stack=50 #-}


module ViperVM.Arch.Linux.Input.Keys
   ( KeymapEntry(..)
   , KeymapFlags(..)
   , getKeyCode
   , setKeyCode
   , getDeviceKeys
   )
where

import Data.Word
import qualified Data.ByteString as BS
import Foreign.Storable
import Foreign.CStorable
import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Utils.BitSet

import Data.Vector.Fixed.Cont (S,Z)
import Data.Vector.Fixed.Storable (Vec)

type N32 = -- 32 
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z
   )))))))))))))))))))))))))))))))

-- | Query or modify keymap data
--
-- `struct input_keymap_entry` in C header file
data KeymapEntry = KeymapEntry
   { keymapEntryFlags    :: Word8                        -- ^ Indicate how kernel should handle the request
   , keymapEntryLength   :: Word8                        -- ^ Length of the scancode
   , keymapEntryIndex    :: Word16                       -- ^ Index in the keymap (may be used instead of the scancode)
   , keymapEntryKeyCode  :: Word32                       -- ^ Key code assigned to this scancode
   , keymapEntryScanCode :: StorableWrap (Vec N32 Word8) -- ^ Scan in machine-endian form (up to 32 bytes)
   } deriving (Generic)


instance CStorable KeymapEntry
instance Storable KeymapEntry where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

data KeymapFlags
   = KeymapByIndex
   deriving (Eq,Show,Enum)

instance EnumBitSet KeymapFlags

-- | Get key code
--
-- EVIOCGKEYCODE_V2
getKeyCode :: IOCTL -> FileDescriptor -> SysRet KeymapEntry
getKeyCode ioctl = ioctlRead ioctl 0x45 0x04 defaultCheck

-- | Set key code
--
-- EVIOCSKEYCODE_V2
setKeyCode :: IOCTL -> FileDescriptor -> KeymapEntry -> SysRet ()
setKeyCode ioctl = ioctlWrite ioctl 0x45 0x04 defaultCheckRet


-- | Get keys (one bit per pressed key)
--
-- EVIOCGKEY
getDeviceKeys :: IOCTL -> Int -> FileDescriptor -> SysRet BS.ByteString
getDeviceKeys ioctl n fd = fmap snd <$> ioctlReadByteString ioctl 0x45 0x18 defaultCheck ((n `div` 8) + 1) fd

