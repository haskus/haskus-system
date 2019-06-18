{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ELF preheader: doesn't depend on the arch
module Haskus.Format.Elf.PreHeader
   ( PreHeader (..)
   , WordSize (..)
   , Endianness (..)
   , OSABI (..)
   , getPreHeader
   , putPreHeader
   -- * Internal
   , getGetters
   , getPutters
   , elfCurrentVersion
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Get
import Haskus.Format.Binary.Put
import Haskus.Format.Binary.Endianness
import Haskus.Utils.Flow (when)
import Haskus.Utils.Text (textFormat,hex,(%),unpack)

-- | Pre-header
data PreHeader = PreHeader
   { preHeaderWordSize   :: WordSize      -- ^ Size of a word
   , preHeaderEndianness :: Endianness    -- ^ Endianness
   , preHeaderVersion    :: Word8         -- ^ Version
   , preHeaderOSABI      :: OSABI         -- ^ OS ABI
   , preHeaderABIVersion :: Word8         -- ^ ABI version
   } deriving (Show)

-- | Current version (still 1...)
elfCurrentVersion :: Word8
elfCurrentVersion = 1

-- | Getter for the pre-header
getPreHeader :: Get PreHeader
getPreHeader = do
   -- check magic number (0x7F + "ELF")
   magic <- getWord32be
   when (magic /= 0x7F454C46) $
      error $ unpack $ textFormat("Not a ELF file (invalid magic number: " % hex % ")") magic

   PreHeader
      <$> (getWord8 >>= \case
            1 -> return WordSize32
            2 -> return WordSize64
            _ -> error "Invalid word size")
      <*> (getWord8 >>= \case
            1 -> return LittleEndian
            2 -> return BigEndian
            _ -> error "Invalid encoding")
      <*> (getWord8 >>= \case
            v
               | v == elfCurrentVersion -> return elfCurrentVersion
               | otherwise  -> error $ "Invalid ELF version " ++ show v)
      <*> (toEnum . fromIntegral <$> getWord8)
      <*> (getWord8 <* skip 7) -- skip padding bytes (16 - already read bytes)

-- | Putter for a pre-header
putPreHeader :: PreHeader -> Put
putPreHeader i = do
   -- put magic number (0x7F + "ELF")
   putWord32be 0x7F454C46

   case preHeaderWordSize i of
      WordSize32 -> putWord8 1
      WordSize64 -> putWord8 2

   case preHeaderEndianness i of
      LittleEndian -> putWord8 1
      BigEndian    -> putWord8 2

   putWord8 (preHeaderVersion i)
   putWord8 (fromIntegral . fromEnum . preHeaderOSABI $ i)
   putWord8 (preHeaderABIVersion i)

   -- put padding bytes
   putWord8 0
   putWord8 0
   putWord8 0
   putWord32le 0

-- | ABI
data OSABI
   = ABI_SYSV           -- ^ UNIX System V ABI
   | ABI_HPUX           -- ^ HP-UX
   | ABI_NETBSD         -- ^ NetBSD
   | ABI_LINUX          -- ^ Linux
   | ABI_SOLARIS        -- ^ Sun Solaris
   | ABI_AIX            -- ^ IBM AIX
   | ABI_IRIX           -- ^ SGI Irix
   | ABI_FREEBSD        -- ^ FreeBSD
   | ABI_TRU64          -- ^ Compaq TRU64 UNIX
   | ABI_MODESTO        -- ^ Novell Modesto
   | ABI_OPENBSD        -- ^ OpenBSD
   | ABI_ARM_AEABI      -- ^ ARM EABI
   | ABI_ARM            -- ^ ARM
   | ABI_STANDALONE     -- ^ Standalone (embedded) application
   | ABI_CUSTOM Word8   -- ^ Unknown ABI
   deriving (Show,Eq)

instance Enum OSABI where
   fromEnum x = case x of
      ABI_SYSV          -> 0
      ABI_HPUX          -> 1
      ABI_NETBSD        -> 2
      ABI_LINUX         -> 3
      ABI_SOLARIS       -> 6
      ABI_AIX           -> 7
      ABI_IRIX          -> 8
      ABI_FREEBSD       -> 9
      ABI_TRU64         -> 10
      ABI_MODESTO       -> 11
      ABI_OPENBSD       -> 12
      ABI_ARM_AEABI     -> 64
      ABI_ARM           -> 97
      ABI_STANDALONE    -> 255
      ABI_CUSTOM v      -> fromIntegral v
   toEnum x = case x of
      0   -> ABI_SYSV
      1   -> ABI_HPUX
      2   -> ABI_NETBSD
      3   -> ABI_LINUX
      6   -> ABI_SOLARIS
      7   -> ABI_AIX
      8   -> ABI_IRIX
      9   -> ABI_FREEBSD
      10  -> ABI_TRU64
      11  -> ABI_MODESTO
      12  -> ABI_OPENBSD
      64  -> ABI_ARM_AEABI
      97  -> ABI_ARM
      255 -> ABI_STANDALONE
      v   -> ABI_CUSTOM (fromIntegral v)

-- | Get the getters appropriate for the ELF file
getGetters :: PreHeader -> (Get Word8, Get Word16, Get Word32, Get Word64, Get Word64)
getGetters i = (gw8, gw16, gw32, gw64, gwN)
   where
      ExtendedWordGetters gw8 gw16 gw32 gw64 gwN =
         getExtendedWordGetters (preHeaderEndianness i) (preHeaderWordSize i)

-- | Get the putters appropriate for the ELF file
getPutters :: PreHeader -> (Word8 -> Put, Word16 -> Put, Word32 -> Put, Word64 -> Put, Word64 -> Put)
getPutters i = (pw8, pw16, pw32, pw64, pwN)
   where
      ExtendedWordPutters pw8 pw16 pw32 pw64 pwN = 
         getExtendedWordPutters (preHeaderEndianness i) (preHeaderWordSize i)
