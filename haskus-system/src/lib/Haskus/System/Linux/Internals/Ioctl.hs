{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | IOCTL
module Haskus.System.Linux.Internals.Ioctl
   ( Command (..)
   , Direction(..)
   , CommandType
   , CommandNumber
   , ioctlCommand
   , rawIoctlCommand
   )
where

import Haskus.Binary.BitField
import Haskus.Binary.Enum
import Haskus.Number.Word
import Haskus.Binary.Storable


-- =============================================================
--    From linux/include/uapi/asm-generic/ioctl.h
-- =============================================================


-- ioctl command encoding: 32 bits total, command in lower 16 bits,
-- size of the parameter structure in the lower 14 bits of the
-- upper 16 bits.
-- Encoding the size of the parameter structure in the ioctl request
-- is useful for catching programs compiled with old versions
-- and to avoid overwriting user space outside the user buffer area.
-- The highest 2 bits are reserved for indicating the ``access mode''.
-- NOTE: This limits the max parameter size to 16kB -1 !
-- 

-- | An IOCTL command number
--
-- The fields are just conventional. Some IOCTLs don't respect them (e.g., use
-- direction=None while they read and/or write, use non corresponding arg size,
-- etc.)
newtype Command = Command (BitFields Word32
  '[ BitField 2  "direction" (EnumField Word8 Direction)
   , BitField 14 "size"      Word16
   , BitField 8  "type"      CommandType
   , BitField 8  "number"    CommandNumber
   ]) deriving (Storable)

-- | Command type
type CommandType   = Word8

-- | Command number
type CommandNumber = Word8

-- | Direction of the IOCTL
data Direction
   = None 
   | Write 
   | Read 
   | WriteRead
   deriving (Show,Eq,Enum)

instance CEnum Direction

-- | Encode a command (similar to _IO, _IOR, ... macros)
ioctlCommand :: Direction -> Word8 -> Word8 -> Word -> Command
{-# INLINE ioctlCommand #-}
ioctlCommand dir typ nb sz = Command
   $ updateField @"direction" (toEnumField dir)
   $ updateField @"size"      (fromIntegral sz)
   $ updateField @"type"      typ
   $ updateField @"number"    nb
   $ BitFields 0

-- | Raw IOCTL command
rawIoctlCommand :: Word32 -> Command
rawIoctlCommand = Command . BitFields
