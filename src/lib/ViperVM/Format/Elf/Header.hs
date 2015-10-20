module ViperVM.Format.Elf.Header
   ( Header (..)
   , Type (..)
   , Arch (..)
   , elfCurrentVersion
   , getHeader
   , putHeader
   )
where

import Data.Word
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put
import ViperVM.Format.Elf.PreHeader

-- | ELF Header
-- We use 64 bits fields for both 32 and 64 bit formats. These are truncated or
-- zero-extended in the 32 bits case.
data Header = Header
   { headerType               :: Type     -- ^ Type of file
   , headerArch               :: Arch     -- ^ Target architecture
   , headerVersion            :: Word32   -- ^ Version
   , headerEntryAddress       :: Word64   -- ^ Entry point address (for executable files)
   , headerSegmentTableOffset :: Word64   -- ^ Offset of the segment table
   , headerSectionTableOffset :: Word64   -- ^ Offset of the section table
   , headerFlags              :: Word32   -- ^ Flags
   , headerHeaderSize         :: Word16   -- ^ Size of the header
   , headerSegmentEntrySize   :: Word16   -- ^ Size of a segment entry
   , headerSegmentEntryCount  :: Word16   -- ^ Number of segment entries
   , headerSectionEntrySize   :: Word16   -- ^ Size of a section entry
   , headerSectionEntryCount  :: Word16   -- ^ Number of section entries
   , headerSectionNameIndex   :: Word16   -- ^ Index of the section containing section names
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

getHeader :: PreHeader -> Get Header
getHeader i = do
   let (_,gw16,gw32,_,gwN) = getGetters i

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

putHeader :: PreHeader -> Header -> Put
putHeader i h = do
   let (_,pw16,pw32,_, pwN) = getPutters i

   pw16 (fromIntegral . fromEnum . headerType $ h)
   pw16 (fromIntegral . fromEnum . headerArch $ h)
   pw32 (headerVersion h)
   pwN  (headerEntryAddress h)
   pwN  (headerSegmentTableOffset h)
   pwN  (headerSectionTableOffset h)
   pw32 (headerFlags h)
   pw16 (headerHeaderSize h)
   pw16 (headerSegmentEntrySize h)
   pw16 (headerSegmentEntryCount h)
   pw16 (headerSectionEntrySize h)
   pw16 (headerSectionEntryCount h)
   pw16 (headerSectionNameIndex h)



