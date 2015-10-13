module ViperVM.Format.Elf.Section
   ( Section (..)
   , SectionFlag (..)
   , SectionType (..)
   , SectionIndex
   , SectionFlags
   , getSection
   , putSection
   , SectionCompression (..)
   , CompressionType (..)
   , getSectionCompression
   , putSectionCompression
   -- * Symbols
   , SymbolEntry (..)
   , SymbolBinding (..)
   , SymbolType (..)
   , SymbolVisibility (..)
   , SymbolInfo (..)
   , getSymbolEntry
   , putSymbolEntry
   -- * Relocation
   , RelocationEntry (..)
   , getRelocationEntry
   , putRelocationEntry
   -- * Internal
   , parseSectionTable
   )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Int
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

import ViperVM.Utils.BitSet (EnumBitSet,BitSet)
import qualified ViperVM.Utils.BitSet as BitSet

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Relocations

type SectionIndex = Word32

data Section = Section
   { sectionNameIndex :: SectionIndex
   , sectionType      :: SectionType
   , sectionFlags     :: SectionFlags
   , sectionAddr      :: Word64
   , sectionOffset    :: Word64
   , sectionSize      :: Word64
   , sectionLink      :: Word32
   , sectionInfo      :: Word32
   , sectionAlignment :: Word64
   , sectionEntrySize :: Word64
   } deriving (Show)

parseSectionTable :: ByteString -> Header -> PreHeader -> [Section]
parseSectionTable bs h pre = fmap f offs
   where
      f o  = runGet (getSection pre) (LBS.drop o bs')
      off  = fromIntegral $ headerSectionTableOffset h
      bs'  = LBS.drop off bs
      sz   = fromIntegral $ headerSectionEntrySize h
      cnt  = fromIntegral $ headerSectionEntryCount h
      offs = [ sz * i | i <- [0..cnt-1]]

getSection :: PreHeader -> Get Section
getSection i = do
   let (_,gw32,_,gwN) = getGetters i
   Section
      <$> gw32
      <*> (toEnum . fromIntegral <$> gw32)
      <*> (BitSet.fromBits <$> gwN)
      <*> gwN
      <*> gwN
      <*> gwN
      <*> gw32
      <*> gw32
      <*> gwN
      <*> gwN

putSection :: PreHeader -> Section -> Put
putSection i s = do
   let (_,pw32,_,pwN) = getPutters i

   pw32 (sectionNameIndex s)
   pw32 (fromIntegral . fromEnum . sectionType $ s)
   pwN  (BitSet.toBits (sectionFlags s))
   pwN  (sectionAddr s)
   pwN  (sectionOffset s)
   pwN  (sectionSize s)
   pw32 (sectionLink s)
   pw32 (sectionInfo s)
   pwN  (sectionAlignment s)
   pwN  (sectionEntrySize s)

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
   deriving (Show, Eq)

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
   | SectionFlagCompressed        -- ^ Section with compressed data
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
      SectionFlagCompressed         -> 11
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
      11 -> SectionFlagCompressed
      30 -> SectionFlagOrdered
      31 -> SectionFlagExclude
      v  -> SectionFlagOther (fromIntegral v)

instance EnumBitSet SectionFlag

type SectionFlags = BitSet Word64 SectionFlag


data CompressionType
   = CompressionZLIB
   | CompressionUnknown Word32
   deriving (Show)

instance Enum CompressionType where
   fromEnum x = case x of
      CompressionZLIB      -> 1
      CompressionUnknown v -> fromIntegral v
   
   toEnum x = case x of
      1 -> CompressionZLIB
      v -> CompressionUnknown (fromIntegral v)


data SectionCompression = SectionCompression
   { sectionCompressionType       :: CompressionType  -- ^ Compression type
   , sectionCompressionSize       :: Word64           -- ^ Uncompressed data size
   , sectionCompressionAlignement :: Word64           -- ^ Uncompressed data alignment
   } deriving (Show)

getSectionCompression :: PreHeader -> Get SectionCompression
getSectionCompression i = do
   let (_,gw32,gw64,_) = getGetters i
   case preHeaderWordSize i of
      WordSize32 -> SectionCompression
         <$> fmap (toEnum . fromIntegral) gw32
         <*> fmap fromIntegral gw32
         <*> fmap fromIntegral gw32
      WordSize64 -> SectionCompression
         <$> fmap (toEnum . fromIntegral) (gw32 <* skip 4)
         <*> gw64
         <*> gw64

putSectionCompression :: PreHeader -> SectionCompression -> Put
putSectionCompression i (SectionCompression typ sz align) = do
   let (_,pw32,pw64,_) = getPutters i
   case preHeaderWordSize i of
      WordSize32 -> do
         pw32 (fromIntegral (fromEnum typ))
         pw32 (fromIntegral sz)
         pw32 (fromIntegral align)
      WordSize64 -> do
         pw32 (fromIntegral (fromEnum typ))
         pw32 0 -- reserved word
         pw64 sz
         pw64 align


data SymbolEntry = SymbolEntry
   { symbolNameIndex    :: Word32
   , symbolBinding      :: SymbolBinding
   , symbolType         :: SymbolType
   , symbolVisibility   :: SymbolVisibility
   , symbolInfo         :: SymbolInfo
   , symbolValue        :: Word64
   , symbolSize         :: Word64
   } deriving (Show)

data SymbolBinding
   = SymbolBindingLocal
   | SymbolBindingGlobal
   | SymbolBindingWeak
   | SymbolBindingUnknown Word8
   deriving (Show)

instance Enum SymbolBinding where
   fromEnum x = case x of
      SymbolBindingLocal      -> 0
      SymbolBindingGlobal     -> 1
      SymbolBindingWeak       -> 2
      SymbolBindingUnknown v  -> fromIntegral v

   toEnum x = case x of
      0 -> SymbolBindingLocal
      1 -> SymbolBindingGlobal
      2 -> SymbolBindingWeak
      v -> SymbolBindingUnknown (fromIntegral v)

data SymbolType
   = SymbolTypeNone
   | SymbolTypeData
   | SymbolTypeCode
   | SymbolTypeSection
   | SymbolTypeFile
   | SymbolTypeCommonData
   | SymbolTypeTLSData
   | SymbolTypeUnknown Word8
   deriving (Show)

instance Enum SymbolType where
   fromEnum x = case x of
      SymbolTypeNone          -> 0
      SymbolTypeData          -> 1
      SymbolTypeCode          -> 2
      SymbolTypeSection       -> 3
      SymbolTypeFile          -> 4
      SymbolTypeCommonData    -> 5
      SymbolTypeTLSData       -> 6
      SymbolTypeUnknown v     -> fromIntegral v

   toEnum x = case x of
      0 -> SymbolTypeNone
      1 -> SymbolTypeData
      2 -> SymbolTypeCode
      3 -> SymbolTypeSection
      4 -> SymbolTypeFile
      5 -> SymbolTypeCommonData
      6 -> SymbolTypeTLSData
      v -> SymbolTypeUnknown (fromIntegral v)

data SymbolVisibility
   = SymbolVisibilityDefault
   | SymbolVisibilityInternal
   | SymbolVisibilityHidden
   | SymbolVisibilityProtected
   deriving (Show)

instance Enum SymbolVisibility where
   fromEnum x = case x of
      SymbolVisibilityDefault    -> 0
      SymbolVisibilityInternal   -> 1
      SymbolVisibilityHidden     -> 2
      SymbolVisibilityProtected  -> 3

   toEnum x = case x of
      0 -> SymbolVisibilityDefault
      1 -> SymbolVisibilityInternal
      2 -> SymbolVisibilityHidden
      3 -> SymbolVisibilityProtected
      v -> error $ "Invalid symbol visibility: " ++ show v

-- | Symbol information
--
-- In the original semantics, symbol's "section" field can be used to encode
-- other information and "info" field is taken for binding/type... We fix this
-- by using "Info" for "section" and Binding/Type for "info".
data SymbolInfo
   = SymbolInfoUndefined             -- ^ Undefined section
   | SymbolInfoAbsolute              -- ^ Associated symbol is absolute
   | SymbolInfoCommon                -- ^ Associated symbol is common
   | SymbolInfoIndexInExtraTable     -- ^ Index is in extra table
   | SymbolInfoSectionBeforeAll      -- ^ Order section before all others (Solaris)
   | SymbolInfoSectionAfterAll       -- ^ Order section after all others (Solaris)
   | SymbolInfoSectionIndex Word16   -- ^ Section index
   | SymbolInfoUnknown Word16        -- ^ Unknown information
   deriving (Show)

instance Enum SymbolInfo where
   fromEnum x = case x of
      SymbolInfoUndefined           -> 0
      SymbolInfoAbsolute            -> 0xfff1
      SymbolInfoCommon              -> 0xfff2
      SymbolInfoIndexInExtraTable   -> 0xffff
      SymbolInfoSectionBeforeAll    -> 0xff00
      SymbolInfoSectionAfterAll     -> 0xff01
      SymbolInfoSectionIndex v      -> fromIntegral v
      SymbolInfoUnknown v           -> fromIntegral v

   toEnum x = case x of
      0      -> SymbolInfoUndefined
      0xfff1 -> SymbolInfoAbsolute
      0xfff2 -> SymbolInfoCommon
      0xffff -> SymbolInfoIndexInExtraTable
      0xff00 -> SymbolInfoSectionBeforeAll
      0xff01 -> SymbolInfoSectionAfterAll
      v 
         | v < 0xff00 -> SymbolInfoSectionIndex (fromIntegral v)
         | otherwise  -> SymbolInfoUnknown (fromIntegral v)

getSymbolEntry :: PreHeader -> Get SymbolEntry
getSymbolEntry i = do
   let (gw16,gw32,_,gwN) = getGetters i
   
   (name,value,size,info,other,sec) <- case preHeaderWordSize i of
      WordSize32 -> do
         name  <- gw32
         value <- gwN
         size  <- gwN
         info  <- getWord8
         other <- getWord8
         sec   <- gw16
         return (name,value,size,info,other,sec)

      WordSize64 -> do
         name  <- gw32
         info  <- getWord8
         other <- getWord8
         sec   <- gw16
         value <- gwN
         size  <- gwN
         return (name,value,size,info,other,sec)
   let 
      typ  = toEnum (fromIntegral $ info .&. 0x0f)
      bind = toEnum (fromIntegral $ info `shiftR` 4)
      visi = toEnum (fromIntegral $ other .&. 0x03)
      ifo  = toEnum (fromIntegral sec)
   return (SymbolEntry name bind typ visi ifo value size)

putSymbolEntry :: PreHeader -> SymbolEntry -> Put
putSymbolEntry i (SymbolEntry name bind typ visi ifo value size) = do
   let 
      (pw16,pw32,_,pwN) = getPutters i
      info = (fromIntegral (fromEnum bind) `shiftL` 4) 
         .|. (fromIntegral (fromEnum typ) .&. 0x0f)
      other = fromIntegral (fromEnum visi) .&. 0x03
      sec = fromIntegral (fromEnum ifo)
   
   case preHeaderWordSize i of
      WordSize32 -> do
         pw32 name
         pwN value
         pwN size
         putWord8 info
         putWord8 other
         pw16 sec

      WordSize64 -> do
         pw32 name
         putWord8 info
         putWord8 other
         pw16 sec
         pwN value
         pwN size


data RelocationEntry = RelocationEntry
   { relocAddress       :: Word64
   , relocType          :: RelocationType
   , relocSymbolIndex   :: Word32
   , relocAddend        :: Maybe Int64
   }
   deriving (Show)


getRelocationEntry :: PreHeader -> Header -> Bool -> Get RelocationEntry
getRelocationEntry i h withAddend = do
   let (_,_,_,gwN) = getGetters i
   
   addr <- gwN
   info <- gwN
   let
      typ = toRelocType (headerArch h) $ case preHeaderWordSize i of
         WordSize32 -> fromIntegral (info .&. 0xff)
         WordSize64 -> fromIntegral (info .&. 0xffffffff)

      sym = case preHeaderWordSize i of
         WordSize32 -> fromIntegral (info `shiftR` 8)
         WordSize64 -> fromIntegral (info `shiftR` 32)

   ad <- if withAddend
      then (Just . fromIntegral <$> gwN) 
      else return Nothing

   return $ RelocationEntry addr typ sym ad

putRelocationEntry :: PreHeader -> Bool -> RelocationEntry -> Put
putRelocationEntry i withAddend rel = do
   let 
      (_,_,_,pwN) = getPutters i
      sym = relocSymbolIndex rel
      typ = fromRelocType (relocType rel)
      info = case preHeaderWordSize i of
         WordSize32 -> (fromIntegral sym `shiftL` 8) 
                       .|. (fromIntegral typ .&. 0xff)
         WordSize64 -> (fromIntegral sym `shiftL` 32) 
                       .|. (fromIntegral typ .&. 0xffffffff)

   pwN (relocAddress rel)
   pwN info
   case (withAddend, relocAddend rel) of
      (True, Just x)   -> pwN (fromIntegral x)
      (False, Nothing) -> return ()
      _                -> error "Addend not found"

