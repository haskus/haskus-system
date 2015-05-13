{-# LANGUAGE LambdaCase #-}
module ViperVM.Format.Elf
   ( module X
   -- ** Pre-Header
   , Info (..)
   , WordSize (..)
   , Endianness (..)
   , OSABI (..)
   , getInfo
   , putInfo
   -- ** Header
   , Header (..)
   , Type (..)
   , Arch (..)
   , elfCurrentVersion
   , getHeader
   , putHeader
   -- ** Section
   , Section (..)
   , SectionFlag (..)
   , SectionFlags
   , getSection
   , putSection
   )
where

import Data.Elf as X

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad (when)

import ViperVM.Arch.Common.Endianness
import ViperVM.Utils.BitSet (EnumBitSet,BitSet)
import qualified ViperVM.Utils.BitSet as BitSet

import Text.Printf

-----------------------------------------------------
-- Info
-----------------------------------------------------

data Info = Info
   { infoWordSize   :: WordSize
   , infoEncoding   :: Endianness
   , infoVersion    :: Word8
   , infoOSABI      :: OSABI
   , infoABIVersion :: Word8
   } deriving (Show)

elfCurrentVersion :: Word8
elfCurrentVersion = 1

getInfo :: Get Info
getInfo = do
   -- check magic number (0x7F + "ELF")
   magic <- getWord32be
   when (magic /= 0x7F454C46) $
      error $ printf "Not a ELF file (invalid magic number: %x)" magic

   Info
      <$> (getWord8 >>= \case
            1 -> return WordSize32
            2 -> return WordSize64
            _ -> error "Invalid word size")
      <*> (getWord8 >>= \case
            1 -> return LittleEndian
            2 -> return BigEndian
            _ -> error "Invalid encoding")
      <*> (getWord8 >>= \case
            1 -> return 1
            v -> error $ "Invalid ELF version " ++ show v)
      <*> (toEnum . fromIntegral <$> getWord8)
      <*> (getWord8 <* skip 7) -- skip padding bytes (16 - already read bytes)


putInfo :: Info -> Put
putInfo i = do
   putWord32be 0x7F454C46

   case infoWordSize i of
      WordSize32 -> putWord8 1
      WordSize64 -> putWord8 2

   case infoEncoding i of
      LittleEndian -> putWord8 1
      BigEndian    -> putWord8 2

   putWord8 (infoVersion i)
   putWord8 (fromIntegral . fromEnum . infoOSABI $ i)
   putWord8 (infoABIVersion i)

   -- put padding bytes
   putWord8 0
   putWord8 0
   putWord8 0
   putWord32le 0



data WordSize
   = WordSize32
   | WordSize64
   deriving (Show, Eq)

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


getGetters :: Info -> (Get Word16, Get Word32, Get Word64, Get Word64)
getGetters i = (gw16, gw32, gw64, gwN)
   where
      (gw16,gw32,gw64) = case infoEncoding i of
         LittleEndian -> (getWord16le, getWord32le, getWord64le)
         BigEndian    -> (getWord16be, getWord32be, getWord64be)

      gwN = case infoWordSize i of
         WordSize32 -> fromIntegral <$> gw32
         WordSize64 -> gw64

getPutters :: Info -> (Word16 -> Put, Word32 -> Put, Word64 -> Put, Word64 -> Put)
getPutters i = (pw16, pw32, pw64, pwN)
   where
      (pw16,pw32,pw64) = case infoEncoding i of
         LittleEndian -> (putWord16le, putWord32le, putWord64le)
         BigEndian    -> (putWord16be, putWord32be, putWord64be)

      pwN = case infoWordSize i of
         WordSize32 -> pw32 . fromIntegral
         WordSize64 -> pw64

-----------------------------------------------------
-- Header
-----------------------------------------------------


getHeader :: Info -> Get Header
getHeader i = do
   let (gw16,gw32,_,gwN) = getGetters i

   Header
      <$> (toEnum . fromIntegral <$> gw16)
      <*> (toEnum . fromIntegral <$> gw16)
      <*> gw32
      <*> gwN
      <*> gwN
      <*> gwN
      <*> gw32
      <*> gw16
      <*> gw16
      <*> gw16
      <*> gw16
      <*> gw16
      <*> gw16

putHeader :: Info -> Header -> Put
putHeader i h = do
   let (pw16,pw32,_, pwN) = getPutters i

   pw16 (fromIntegral . fromEnum . headerType $ h)
   pw16 (fromIntegral . fromEnum . headerArch $ h)
   pw32 (headerVersion h)
   pwN  (headerEntry h)
   pwN  (headerSegmentTableOffset h)
   pwN  (headerSectionTableOffset h)
   pw32 (headerFlags h)
   pw16 (headerHeaderSize h)
   pw16 (headerSegmentEntrySize h)
   pw16 (headerSegmentEntryCount h)
   pw16 (headerSectionEntrySize h)
   pw16 (headerSectionEntryCount h)
   pw16 (headerSectionNameIndex h)


-- | Header
-- We use 64 bits fields for both 32 and 64 bit formats. These are truncated or
-- zero-extended in case of 32 bit.
data Header = Header
   { headerType               :: Type
   , headerArch               :: Arch
   , headerVersion            :: Word32
   , headerEntry              :: Word64
   , headerSegmentTableOffset :: Word64
   , headerSectionTableOffset :: Word64
   , headerFlags              :: Word32
   , headerHeaderSize         :: Word16
   , headerSegmentEntrySize   :: Word16
   , headerSegmentEntryCount  :: Word16
   , headerSectionEntrySize   :: Word16
   , headerSectionEntryCount  :: Word16
   , headerSectionNameIndex   :: Word16
   } deriving (Show)

data Type
   = TypeNone           -- ^ No file type
   | TypeRelocatable    -- ^ Relocatable file
   | TypeExecutable     -- ^ Executable file
   | TypeSharedObject   -- ^ Shared object file
   | TypeCoreFile       -- ^ Core file
   deriving (Show,Eq,Enum)

data Arch
   = ArchNone         -- ^ No machine
   | ArchM32          -- ^ AT&T WE 32100
   | ArchSPARC        -- ^ SUN SPARC
   | Arch386          -- ^ Intel 80386
   | Arch68K          -- ^ Motorola m68k family
   | Arch88K          -- ^ Motorola m88k family
   | Arch860          -- ^ Intel 80860
   | ArchMIPS         -- ^ MIPS R3000 big-endian
   | ArchS370         -- ^ IBM System/370
   | ArchMIPS_RS3_LE  -- ^ MIPS R3000 little-endian
   | ArchPARISC       -- ^ HPPA
   | ArchVPP500       -- ^ Fujitsu VPP500
   | ArchSPARC32PLUS  -- ^ Sun's "v8plus"
   | Arch960          -- ^ Intel 80960
   | ArchPPC          -- ^ PowerPC
   | ArchPPC64        -- ^ PowerPC 64-bit
   | ArchS390         -- ^ IBM S390
   | ArchV800         -- ^ NEC V800 series
   | ArchFR20         -- ^ Fujitsu FR20
   | ArchRH32         -- ^ TRW RH-32
   | ArchRCE          -- ^ Motorola RCE
   | ArchARM          -- ^ ARM
   | ArchFAKE_ALPHA   -- ^ Digital Alpha
   | ArchSH           -- ^ Hitachi SH
   | ArchSPARCV9      -- ^ SPARC v9 64-bit
   | ArchTRICORE      -- ^ Siemens Tricore
   | ArchARC          -- ^ Argonaut RISC Core
   | ArchH8_300       -- ^ Hitachi H8/300
   | ArchH8_300H      -- ^ Hitachi H8/300H
   | ArchH8S          -- ^ Hitachi H8S
   | ArchH8_500       -- ^ Hitachi H8/500
   | ArchIA_64        -- ^ Intel Merced
   | ArchMIPS_X       -- ^ Stanford MIPS-X
   | ArchCOLDFIRE     -- ^ Motorola Coldfire
   | Arch68HC12       -- ^ Motorola M68HC12
   | ArchMMA          -- ^ Fujitsu MMA Multimedia Accelerator
   | ArchPCP          -- ^ Siemens PCP
   | ArchNCPU         -- ^ Sony nCPU embeeded RISC
   | ArchNDR1         -- ^ Denso NDR1 microprocessor
   | ArchSTARCORE     -- ^ Motorola Start*Core processor
   | ArchME16         -- ^ Toyota ME16 processor
   | ArchST100        -- ^ STMicroelectronic ST100 processor
   | ArchTINYJ        -- ^ Advanced Logic Corp. Tinyj emb.fam
   | ArchX86_64       -- ^ AMD x86-64 architecture
   | ArchPDSP         -- ^ Sony DSP Processor
   | ArchFX66         -- ^ Siemens FX66 microcontroller
   | ArchST9PLUS      -- ^ STMicroelectronics ST9+ 8/16 mc
   | ArchST7          -- ^ STmicroelectronics ST7 8 bit mc
   | Arch68HC16       -- ^ Motorola MC68HC16 microcontroller
   | Arch68HC11       -- ^ Motorola MC68HC11 microcontroller
   | Arch68HC08       -- ^ Motorola MC68HC08 microcontroller
   | Arch68HC05       -- ^ Motorola MC68HC05 microcontroller
   | ArchSVX          -- ^ Silicon Graphics SVx
   | ArchST19         -- ^ STMicroelectronics ST19 8 bit mc
   | ArchVAX          -- ^ Digital VAX
   | ArchCRIS         -- ^ Axis Communications 32-bit embedded processor
   | ArchJAVELIN      -- ^ Infineon Technologies 32-bit embedded processor
   | ArchFIREPATH     -- ^ Element 14 64-bit DSP Processor
   | ArchZSP          -- ^ LSI Logic 16-bit DSP Processor
   | ArchMMIX         -- ^ Donald Knuth's educational 64-bit processor
   | ArchHUANY        -- ^ Harvard University machine-independent object files
   | ArchPRISM        -- ^ SiTera Prism
   | ArchAVR          -- ^ Atmel AVR 8-bit microcontroller
   | ArchFR30         -- ^ Fujitsu FR30
   | ArchD10V         -- ^ Mitsubishi D10V
   | ArchD30V         -- ^ Mitsubishi D30V
   | ArchV850         -- ^ NEC v850
   | ArchM32R         -- ^ Mitsubishi M32R
   | ArchMN10300      -- ^ Matsushita MN10300
   | ArchMN10200      -- ^ Matsushita MN10200
   | ArchPJ           -- ^ picoJava
   | ArchOPENRISC     -- ^ OpenRISC 32-bit embedded processor
   | ArchARC_A5       -- ^ ARC Cores Tangent-A5
   | ArchXTENSA       -- ^ Tensilica Xtensa Architecture
   | ArchALTERA_NIOS2 -- ^ Altera Nios II
   | ArchAARCH64      -- ^ ARM AARCH64
   | ArchTILEPRO      -- ^ Tilera TILEPro
   | ArchMICROBLAZE   -- ^ Xilinx MicroBlaze
   | ArchTILEGX       -- ^ Tilera TILE-Gx
   | ArchCustom Word16
   deriving (Show,Eq)

instance Enum Arch where
   fromEnum x = case x of
      ArchNone         -> 0
      ArchM32          -> 1
      ArchSPARC        -> 2
      Arch386          -> 3
      Arch68K          -> 4
      Arch88K          -> 5
      Arch860          -> 7
      ArchMIPS         -> 8
      ArchS370         -> 9
      ArchMIPS_RS3_LE  -> 10
      ArchPARISC       -> 15
      ArchVPP500       -> 17
      ArchSPARC32PLUS  -> 18
      Arch960          -> 19
      ArchPPC          -> 20
      ArchPPC64        -> 21
      ArchS390         -> 22
      ArchV800         -> 36
      ArchFR20         -> 37
      ArchRH32         -> 38
      ArchRCE          -> 39
      ArchARM          -> 40
      ArchFAKE_ALPHA   -> 41
      ArchSH           -> 42
      ArchSPARCV9      -> 43
      ArchTRICORE      -> 44
      ArchARC          -> 45
      ArchH8_300       -> 46
      ArchH8_300H      -> 47
      ArchH8S          -> 48
      ArchH8_500       -> 49
      ArchIA_64        -> 50
      ArchMIPS_X       -> 51
      ArchCOLDFIRE     -> 52
      Arch68HC12       -> 53
      ArchMMA          -> 54
      ArchPCP          -> 55
      ArchNCPU         -> 56
      ArchNDR1         -> 57
      ArchSTARCORE     -> 58
      ArchME16         -> 59
      ArchST100        -> 60
      ArchTINYJ        -> 61
      ArchX86_64       -> 62
      ArchPDSP         -> 63
      ArchFX66         -> 66
      ArchST9PLUS      -> 67
      ArchST7          -> 68
      Arch68HC16       -> 69
      Arch68HC11       -> 70
      Arch68HC08       -> 71
      Arch68HC05       -> 72
      ArchSVX          -> 73
      ArchST19         -> 74
      ArchVAX          -> 75
      ArchCRIS         -> 76
      ArchJAVELIN      -> 77
      ArchFIREPATH     -> 78
      ArchZSP          -> 79
      ArchMMIX         -> 80
      ArchHUANY        -> 81
      ArchPRISM        -> 82
      ArchAVR          -> 83
      ArchFR30         -> 84
      ArchD10V         -> 85
      ArchD30V         -> 86
      ArchV850         -> 87
      ArchM32R         -> 88
      ArchMN10300      -> 89
      ArchMN10200      -> 90
      ArchPJ           -> 91
      ArchOPENRISC     -> 92
      ArchARC_A5       -> 93
      ArchXTENSA       -> 94
      ArchALTERA_NIOS2 -> 113
      ArchAARCH64      -> 183
      ArchTILEPRO      -> 188
      ArchMICROBLAZE   -> 189
      ArchTILEGX       -> 191
      ArchCustom v     -> fromIntegral v
   toEnum x = case x of
      0   -> ArchNone
      1   -> ArchM32
      2   -> ArchSPARC
      3   -> Arch386
      4   -> Arch68K
      5   -> Arch88K
      7   -> Arch860
      8   -> ArchMIPS
      9   -> ArchS370
      10  -> ArchMIPS_RS3_LE
      15  -> ArchPARISC
      17  -> ArchVPP500
      18  -> ArchSPARC32PLUS
      19  -> Arch960
      20  -> ArchPPC
      21  -> ArchPPC64
      22  -> ArchS390
      36  -> ArchV800
      37  -> ArchFR20
      38  -> ArchRH32
      39  -> ArchRCE
      40  -> ArchARM
      41  -> ArchFAKE_ALPHA
      42  -> ArchSH
      43  -> ArchSPARCV9
      44  -> ArchTRICORE
      45  -> ArchARC
      46  -> ArchH8_300
      47  -> ArchH8_300H
      48  -> ArchH8S
      49  -> ArchH8_500
      50  -> ArchIA_64
      51  -> ArchMIPS_X
      52  -> ArchCOLDFIRE
      53  -> Arch68HC12
      54  -> ArchMMA
      55  -> ArchPCP
      56  -> ArchNCPU
      57  -> ArchNDR1
      58  -> ArchSTARCORE
      59  -> ArchME16
      60  -> ArchST100
      61  -> ArchTINYJ
      62  -> ArchX86_64
      63  -> ArchPDSP
      66  -> ArchFX66
      67  -> ArchST9PLUS
      68  -> ArchST7
      69  -> Arch68HC16
      70  -> Arch68HC11
      71  -> Arch68HC08
      72  -> Arch68HC05
      73  -> ArchSVX
      74  -> ArchST19
      75  -> ArchVAX
      76  -> ArchCRIS
      77  -> ArchJAVELIN
      78  -> ArchFIREPATH
      79  -> ArchZSP
      80  -> ArchMMIX
      81  -> ArchHUANY
      82  -> ArchPRISM
      83  -> ArchAVR
      84  -> ArchFR30
      85  -> ArchD10V
      86  -> ArchD30V
      87  -> ArchV850
      88  -> ArchM32R
      89  -> ArchMN10300
      90  -> ArchMN10200
      91  -> ArchPJ
      92  -> ArchOPENRISC
      93  -> ArchARC_A5
      94  -> ArchXTENSA
      113 -> ArchALTERA_NIOS2
      183 -> ArchAARCH64
      188 -> ArchTILEPRO
      189 -> ArchMICROBLAZE
      191 -> ArchTILEGX
      v   -> ArchCustom (fromIntegral v)

-----------------------------------------------------
-- Sections
-----------------------------------------------------

data Section = Section
   { sectionNameIndex :: Word64
   , sectionType      :: SectionType
   , sectionFlags     :: SectionFlags
   , sectionAddr      :: Word64
   , sectionOffset    :: Word64
   , sectionSize      :: Word64
   , sectionLink      :: Word64
   , sectionInfo      :: Word64
   , sectionAlignment :: Word64
   , sectionEntrySize :: Word64
   } deriving (Show)

getSection :: Info -> Get Section
getSection i = do
   let (_,_,_,gwN) = getGetters i
   Section
      <$> gwN
      <*> (toEnum . fromIntegral <$> gwN)
      <*> (BitSet.fromBits <$> gwN)
      <*> gwN
      <*> gwN
      <*> gwN
      <*> gwN
      <*> gwN
      <*> gwN
      <*> gwN

putSection :: Info -> Section -> Put
putSection i s = do
   let (_,_,_, pwN) = getPutters i

   pwN (sectionNameIndex s)
   pwN (fromIntegral . fromEnum . sectionType $ s)
   pwN (BitSet.toBits (sectionFlags s))
   pwN (sectionAddr s)
   pwN (sectionOffset s)
   pwN (sectionSize s)
   pwN (sectionLink s)
   pwN (sectionInfo s)
   pwN (sectionAlignment s)
   pwN (sectionEntrySize s)

data SectionType
   = SectionTypeNone                    -- ^ Section header table entry unused
   | SectionTypePROGBITS                -- ^ Program data
   | SectionTypeSYMTAB                  -- ^ Symbol table
   | SectionTypeSTRTAB                  -- ^ String table
   | SectionTypeRELA                    -- ^ Relocation entries with addends
   | SectionTypeHASH                    -- ^ Symbol hash table
   | SectionTypeDYNAMIC                 -- ^ Dynamic linking information
   | SectionTypeNOTE                    -- ^ Notes
   | SectionTypeNOBITS                  -- ^ Program space with no data (bss)
   | SectionTypeREL                     -- ^ Relocation entries, no addends
   | SectionTypeSHLIB                   -- ^ Reserved
   | SectionTypeDYNSYM                  -- ^ Dynamic linker symbol table
   | SectionTypeINIT_ARRAY              -- ^ Array of constructors
   | SectionTypeFINI_ARRAY              -- ^ Array of destructors
   | SectionTypePREINIT_ARRAY           -- ^ Array of pre-constructors
   | SectionTypeGROUP                   -- ^ Section group
   | SectionTypeSYMTAB_SHNDX            -- ^ Extended section indeces

   | SectionTypeGNU_ATTRIBUTES          -- ^ Object attributes.
   | SectionTypeGNU_HASH                -- ^ GNU-style hash table.
   | SectionTypeGNU_LIBLIST             -- ^ Prelink library list
   | SectionTypeCHECKSUM                -- ^ Checksum for DSO content.
   | SectionTypeSUNW_move               
   | SectionTypeSUNW_COMDAT             
   | SectionTypeSUNW_syminfo            
   | SectionTypeGNU_verdef              -- ^ Version definition section.
   | SectionTypeGNU_verneed             -- ^ Version needs section.
   | SectionTypeGNU_versym              -- ^ Version symbol table.
   | SectionTypeCustom Word64
   deriving (Show)

instance Enum SectionType where
   fromEnum x = case x of
      SectionTypeNone            -> 0
      SectionTypePROGBITS        -> 1
      SectionTypeSYMTAB          -> 2
      SectionTypeSTRTAB          -> 3
      SectionTypeRELA            -> 4
      SectionTypeHASH            -> 5
      SectionTypeDYNAMIC         -> 6
      SectionTypeNOTE            -> 7
      SectionTypeNOBITS          -> 8
      SectionTypeREL             -> 9
      SectionTypeSHLIB           -> 10
      SectionTypeDYNSYM          -> 11
      SectionTypeINIT_ARRAY      -> 14
      SectionTypeFINI_ARRAY      -> 15
      SectionTypePREINIT_ARRAY   -> 16
      SectionTypeGROUP           -> 17
      SectionTypeSYMTAB_SHNDX    -> 18
      SectionTypeGNU_ATTRIBUTES  -> 0x6ffffff5
      SectionTypeGNU_HASH        -> 0x6ffffff6
      SectionTypeGNU_LIBLIST     -> 0x6ffffff7
      SectionTypeCHECKSUM        -> 0x6ffffff8
      SectionTypeSUNW_move       -> 0x6ffffffa
      SectionTypeSUNW_COMDAT     -> 0x6ffffffb
      SectionTypeSUNW_syminfo    -> 0x6ffffffc
      SectionTypeGNU_verdef      -> 0x6ffffffd
      SectionTypeGNU_verneed     -> 0x6ffffffe
      SectionTypeGNU_versym      -> 0x6fffffff
      SectionTypeCustom v        -> fromIntegral v
   toEnum x = case x of
      0           -> SectionTypeNone
      1           -> SectionTypePROGBITS
      2           -> SectionTypeSYMTAB
      3           -> SectionTypeSTRTAB
      4           -> SectionTypeRELA
      5           -> SectionTypeHASH
      6           -> SectionTypeDYNAMIC
      7           -> SectionTypeNOTE
      8           -> SectionTypeNOBITS
      9           -> SectionTypeREL
      10          -> SectionTypeSHLIB
      11          -> SectionTypeDYNSYM
      14          -> SectionTypeINIT_ARRAY
      15          -> SectionTypeFINI_ARRAY
      16          -> SectionTypePREINIT_ARRAY
      17          -> SectionTypeGROUP
      18          -> SectionTypeSYMTAB_SHNDX
      0x6ffffff5  -> SectionTypeGNU_ATTRIBUTES
      0x6ffffff6  -> SectionTypeGNU_HASH
      0x6ffffff7  -> SectionTypeGNU_LIBLIST
      0x6ffffff8  -> SectionTypeCHECKSUM
      0x6ffffffa  -> SectionTypeSUNW_move
      0x6ffffffb  -> SectionTypeSUNW_COMDAT
      0x6ffffffc  -> SectionTypeSUNW_syminfo
      0x6ffffffd  -> SectionTypeGNU_verdef
      0x6ffffffe  -> SectionTypeGNU_verneed
      0x6fffffff  -> SectionTypeGNU_versym
      v           -> SectionTypeCustom (fromIntegral v)

data SectionFlag
   = SectionFlagWritable          -- ^ Writable
   | SectionFlagAlloc             -- ^ Occupies memory during execution
   | SectionFlagExecutable        -- ^ Executable
   | SectionFlagMergeable         -- ^ Might be merged
   | SectionFlagStrings           -- ^ Contains nul-terminated strings
   | SectionFlagInfoLink          -- ^ `sh_info' contains SHT index
   | SectionFlagPreserveLinkOrder -- ^ Preserve order after combining
   | SectionFlagOS_NonConforming  -- ^ Non-standard OS specific handling required
   | SectionFlagGROUP             -- ^ Section is member of a group.
   | SectionFlagTLS               -- ^ Section hold thread-local data.
   | SectionFlagOrdered           -- ^ Special ordering requirement
   | SectionFlagExclude           -- ^ Section is excluded unless referenced or allocated (Solaris).
   | SectionFlagOther Word        -- ^ Other flags
   deriving (Show,Eq)

instance Enum SectionFlag where
   fromEnum x = case x of
      SectionFlagWritable           -> 0
      SectionFlagAlloc              -> 1
      SectionFlagExecutable         -> 2
      SectionFlagMergeable          -> 4
      SectionFlagStrings            -> 5
      SectionFlagInfoLink           -> 6
      SectionFlagPreserveLinkOrder  -> 7
      SectionFlagOS_NonConforming   -> 8
      SectionFlagGROUP              -> 9
      SectionFlagTLS                -> 10
      SectionFlagOrdered            -> 30
      SectionFlagExclude            -> 31
      SectionFlagOther v            -> fromIntegral v
   toEnum x = case x of
      0  -> SectionFlagWritable
      1  -> SectionFlagAlloc
      2  -> SectionFlagExecutable
      4  -> SectionFlagMergeable
      5  -> SectionFlagStrings
      6  -> SectionFlagInfoLink
      7  -> SectionFlagPreserveLinkOrder
      8  -> SectionFlagOS_NonConforming
      9  -> SectionFlagGROUP
      10 -> SectionFlagTLS
      30 -> SectionFlagOrdered
      31 -> SectionFlagExclude
      v  -> SectionFlagOther (fromIntegral v)

instance EnumBitSet SectionFlag

type SectionFlags = BitSet Word64 SectionFlag
