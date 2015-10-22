module ViperVM.Format.Elf.Section
   ( Section (..)
   , SectionFlag (..)
   , SectionType (..)
   , SectionIndex
   , SectionFlags
   , getSection
   , getFirstSection
   , putSection
   , SectionCompression (..)
   , CompressionType (..)
   , getSectionCompression
   , putSectionCompression
   -- * Internal
   , getSectionTable
   )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put

import ViperVM.Format.Binary.BitSet (EnumBitSet,BitSet)
import qualified ViperVM.Format.Binary.BitSet as BitSet

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header

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

getSectionTable :: ByteString -> Header -> PreHeader -> Vector Section
getSectionTable bs h pre = fmap f offs
   where
      f o  = runGet (getSection pre) (LBS.drop o bs')
      off  = fromIntegral $ headerSectionTableOffset h
      bs'  = LBS.drop off bs
      sz   = fromIntegral $ headerSectionEntrySize h
      cnt  = fromIntegral $ headerSectionEntryCount h
      offs = Vector.fromList [ 0, sz .. (cnt-1) * sz]

-- | Return the first section that can contain special values for segments
getFirstSection :: ByteString -> Header -> PreHeader -> Section
getFirstSection bs hdr pre = runGet (getSection pre) (LBS.drop off bs)
   where
      off  = fromIntegral $ headerSectionTableOffset hdr

getSection :: PreHeader -> Get Section
getSection i = do
   let (_,_,gw32,_,gwN) = getGetters i
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
   let (_,_,pw32,_,pwN) = getPutters i

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

-- | Section flags
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

-- | Compressed section type
data CompressionType
   = CompressionZLIB             -- ^ Section uses ZLIB/Deflate compression
   | CompressionUnknown Word32   -- ^ Unknown compression used
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
   let (_,_,gw32,_,gwN) = getGetters i
   case preHeaderWordSize i of
      WordSize32 -> SectionCompression
         <$> fmap (toEnum . fromIntegral) gw32
         <*> gwN
         <*> gwN
      WordSize64 -> SectionCompression
         <$> fmap (toEnum . fromIntegral) (gw32 <* skip 4)
         <*> gwN
         <*> gwN

putSectionCompression :: PreHeader -> SectionCompression -> Put
putSectionCompression i (SectionCompression typ sz align) = do
   let (_,_,pw32,_,pwN) = getPutters i
   case preHeaderWordSize i of
      WordSize32 -> do
         pw32 (fromIntegral (fromEnum typ))
         pwN sz
         pwN align
      WordSize64 -> do
         pw32 (fromIntegral (fromEnum typ))
         pw32 0 -- reserved word
         pwN sz
         pwN align



