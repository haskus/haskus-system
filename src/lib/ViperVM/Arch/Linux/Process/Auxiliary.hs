{-# LANGUAGE LambdaCase #-}

-- | Auxiliary vector
--
-- The kernel's ELF binary loader passes some information to user space into the
-- "auxiliary vector" when a program is executed. It is just a key-value array
-- stored alongside program parameters and environment.
--
-- The libc exports a "getauxval" function that we use to query a value by key
-- in the auxiliary vector.
module ViperVM.Arch.Linux.Process.Auxiliary
   ( AuxKey (..)
   , getAuxiliaryValue
   -- * Helpers
   , getFileDescriptor
   , getVDSOAddr
   )
where

import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word
import ViperVM.Arch.Linux.Handle

-- =============================================================
--    From linux/include/uapi/linux/auxvec.h
--    and also from: linux/arch/ARCH/include/uapi/asm/auxvec.h
--    (e.g., VDSO address entry)
-- =============================================================

-- | Keys for the entries in the auxiliary table put on the initial stack
data AuxKey
   = AuxFileDescriptor     -- ^ file descriptor of program
   | AuxHeaders            -- ^ program headers for program
   | AuxHeaderEntrySize    -- ^ size of program header entry
   | AuxHenderEntryCount   -- ^ number of program headers
   | AuxPageSize           -- ^ system page size
   | AuxInterpreterAddr    -- ^ base address of interpreter
   | AuxFlags              -- ^ flags
   | AuxEntryPoint         -- ^ entry point of program
   | AuxIsNotELF           -- ^ program is not ELF
   | AuxUID                -- ^ real uid
   | AuxEUID               -- ^ effective uid
   | AuxGID                -- ^ real gid
   | AuxEGID               -- ^ effective gid
   | AuxPlatform           -- ^ string identifying CPU for optimizations
   | AuxHardwareHints      -- ^ arch dependent hints at CPU capabilities
   | AuxClockTickFrequency -- ^ frequency at which times() increments
   | AuxIsSecure           -- ^ secure mode boolean
   | AuxRealPlatform       -- ^ string identifying real platform, may differ from AuxPlatform
   | AuxRandomBytes        -- ^ address of 16 random bytes
   | AuxHardwareHints2     -- ^ extension of AT_HWCAP
   | AuxFilename           -- ^ filename of program
   | AuxVDSOAddr           -- ^ address of the vDSO page
   deriving (Show,Eq,Enum)

instance CEnum AuxKey where
   fromCEnum = \case
      AuxFileDescriptor     -> 2
      AuxHeaders            -> 3
      AuxHeaderEntrySize    -> 4
      AuxHenderEntryCount   -> 5
      AuxPageSize           -> 6
      AuxInterpreterAddr    -> 7
      AuxFlags              -> 8
      AuxEntryPoint         -> 9
      AuxIsNotELF           -> 10
      AuxUID                -> 11
      AuxEUID               -> 12
      AuxGID                -> 13
      AuxEGID               -> 14
      AuxPlatform           -> 15
      AuxHardwareHints      -> 16
      AuxClockTickFrequency -> 17
      AuxIsSecure           -> 23
      AuxRealPlatform       -> 24
      AuxRandomBytes        -> 25
      AuxHardwareHints2     -> 26
      AuxFilename           -> 31
      AuxVDSOAddr           -> 33

   toCEnum = \case
      2   -> AuxFileDescriptor
      3   -> AuxHeaders
      4   -> AuxHeaderEntrySize
      5   -> AuxHenderEntryCount
      6   -> AuxPageSize
      7   -> AuxInterpreterAddr
      8   -> AuxFlags
      9   -> AuxEntryPoint
      10  -> AuxIsNotELF
      11  -> AuxUID
      12  -> AuxEUID
      13  -> AuxGID
      14  -> AuxEGID
      15  -> AuxPlatform
      16  -> AuxHardwareHints
      17  -> AuxClockTickFrequency
      23  -> AuxIsSecure
      24  -> AuxRealPlatform
      25  -> AuxRandomBytes
      26  -> AuxHardwareHints2
      31  -> AuxFilename
      33  -> AuxVDSOAddr
      x   -> error ("Invalid auxiliary vector key ("++ show x' ++")")
               where 
                  x' :: Word
                  x' = fromIntegral x

foreign import ccall unsafe "getauxval"
   getAuxiliaryValue' :: Word64 -> Word64

-- | Get the value associated with a key in the auxiliary vector
getAuxiliaryValue :: AuxKey -> Word64
getAuxiliaryValue = getAuxiliaryValue' . fromCEnum

-- | File decriptor of the program
getFileDescriptor :: Handle
getFileDescriptor = Handle (fromIntegral (getAuxiliaryValue AuxFileDescriptor))

-- | Address of the vDSO page
getVDSOAddr :: Word64
getVDSOAddr = getAuxiliaryValue AuxVDSOAddr
