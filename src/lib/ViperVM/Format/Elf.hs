{-# LANGUAGE LambdaCase #-}
module ViperVM.Format.Elf
   ( Elf (..)
   , parseElf
   , readElf
   , getSectionContentLBS
   , extractSectionStrings
   , extractStringFromSection
   , getSectionNameByIndex
   , getSectionByIndex
   , getSectionNameSection
   , getSectionName
   , getSectionNames
   , getSectionSymbols
   , getRelocationEntries
   , getDynamicEntries
   , getVersionNeededEntries
   , findSectionByName
   , extractZCATable
   , FullSectionType (..)
   , getFullSectionType
   -- * Dynamic section
   , DynamicEntry (..)
   , getDynamicEntry
   -- * Version needed section
   , VersionNeeded (..)
   )
where

import Data.Int
import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.Text.Encoding as Text

import qualified ViperVM.Utils.BitSet as BitSet

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Section
import ViperVM.Format.Elf.Segment
import ViperVM.Format.Elf.Symbol
import ViperVM.Format.Elf.Relocation
import ViperVM.Format.Elf.Dynamic
import ViperVM.Format.Elf.Version
import ViperVM.Format.Elf.Intel

-- | Structure representing a ELF file
data Elf = Elf
   { elfPreHeader :: PreHeader      -- ^ Pre-header
   , elfHeader    :: Header         -- ^ Header
   , elfSections  :: Vector Section -- ^ Sections
   , elfSegments  :: Vector Segment -- ^ Segments
   , elfContent   :: ByteString     -- ^ Whole content
   } deriving (Show)

-- | Parse the ELF format
parseElf :: ByteString -> Elf
parseElf bs = Elf pre hdr sections segments bs
   where
      pre      = runGet getPreHeader bs
      hdr      = runGet (skip 16 >> getHeader pre) bs
      sections = getSectionTable bs hdr pre
      segments = getSegmentTable bs hdr pre

-- | Read a ELF file
readElf :: FilePath -> IO Elf
readElf path = parseElf <$> LBS.readFile path


getSectionContentLBS :: Elf -> Section -> ByteString
getSectionContentLBS elf s = LBS.take sz (LBS.drop off bs)
   where
      bs  = elfContent elf
      sz  = fromIntegral $ sectionSize s 
      off = fromIntegral $ sectionOffset s 
   

extractSectionStrings :: Elf -> Section -> [(Int64,Text)]
extractSectionStrings elf sec = 
      case sectionType sec of
         SectionTypeSTRTAB -> 
            case LBS.last content of
               0 -> sumSzs `zip` ss'
               c   -> error $ "Last symbol in section is not NUL but " ++ show c
         _                 -> error "Invalid section type"
   where
      -- section content
      content = getSectionContentLBS elf sec
      -- bytestring list: drop last \0 and split at every other \0
      ss  = LBS.split 0 (LBS.init content)
      -- string list: convert bytestrings to texts
      ss' = fmap (Text.decodeUtf8 . LBS.toStrict) ss 
      -- raw (bytestring) size for each string: prefix sum
      szs = fmap (\x -> LBS.length x + 1) ss
      sumSzs = scanl (+) 0 szs


extractStringFromSection :: Elf -> Section -> SectionIndex -> Maybe Text
extractStringFromSection elf sec idx = res
   where
      -- Check that section type is valid and index is within range
      res = case (sectionSize sec, sectionType sec) of
         (sz,SectionTypeSTRTAB)
            | sz > fromIntegral idx  -> Just (extractStr sec)
         _                           -> Nothing

      -- extract the string
      extractStr s = Text.decodeUtf8
         $ LBS.toStrict
         $ LBS.takeWhile (/=0)
         $ LBS.drop (fromIntegral idx)
         $ getSectionContentLBS elf s

-- | Get a section by index
getSectionByIndex :: Integral a => Elf -> a -> Maybe Section
getSectionByIndex elf i = 
   elfSections elf Vector.!? fromIntegral i


-- | Return the section containing section names (if any)
getSectionNameSection :: Elf -> Maybe Section
getSectionNameSection elf = do
   -- Find the section containing section names
   let secIdx = headerSectionNameIndex (elfHeader elf)
   getSectionByIndex elf secIdx

getSectionNameByIndex :: Elf -> SectionIndex -> Maybe Text
getSectionNameByIndex elf idx = do
   -- Find the section containing section names
   sec <- getSectionNameSection elf
   -- extract section name for the index idx
   extractStringFromSection elf sec idx

-- | Return the name of a section
getSectionName :: Elf -> Section -> Maybe Text
getSectionName elf = getSectionNameByIndex elf . sectionNameIndex

-- | Return all the section names
getSectionNames :: Elf -> Vector (Section, Maybe Text)
getSectionNames elf = fmap f (elfSections elf)
   where
      f x = (x, getSectionName elf x)

getTable :: Elf -> Section -> [ByteString]
getTable elf sec = bss
   where
      -- content of the section
      content = getSectionContentLBS elf sec
      -- size of a single entry
      size = case fromIntegral (sectionEntrySize sec) of
         0 -> error "Invalid table section: entry size is null"
         x -> x
      -- size of the section
      secsize = fromIntegral (sectionSize sec)
      -- number of entries
      n = if secsize /= 0
         then secsize `div` size
         else 0
      -- offsets in the section
      offs = [0, size .. (n-1) * size]
      -- read entries
      bss = fmap (\off -> LBS.take size $ LBS.drop off content) offs

getSectionSymbols :: Elf -> Section -> [SymbolEntry]
getSectionSymbols elf sec =
      case sectionType sec of
         SectionTypeSYMTAB -> fmap rd bss
         SectionTypeDYNSYM -> fmap rd bss
         _                 -> error "Invalid section type"
   where
      -- get table of bytestrings
      bss = getTable elf sec
      -- read symbol entry
      rd = runGet (getSymbolEntry (elfPreHeader elf))


getRelocationEntries :: Elf -> Section -> [RelocationEntry]
getRelocationEntries elf sec = 
      case sectionType sec of
         SectionTypeREL    -> fmap rel bss
         SectionTypeRELA   -> fmap rela bss
         _                 -> error "Invalid section type"
   where
      -- get table of bytestrings
      bss  = getTable elf sec
      -- read symbol entry
      rel  = runGet (getRelocationEntry (elfPreHeader elf) (elfHeader elf) False)
      rela = runGet (getRelocationEntry (elfPreHeader elf) (elfHeader elf) True)

getDynamicEntries :: Elf -> Section -> [DynamicEntry]
getDynamicEntries elf sec =
      case sectionType sec of
         SectionTypeDYNAMIC -> fmap rd bss
         _                  -> error "Invalid section type"
   where
      -- get table of bytestrings
      bss = getTable elf sec
      -- read symbol entry
      rd = runGet (getDynamicEntry elf sec)

data VersionNeeded = VersionNeeded
   { vnVersion       :: VersionNeededVersion       -- ^ Version of structure
   , vnFileName      :: Text                       -- ^ Dependency file name
   , vnEntries       :: [VersionNeededAuxiliary]   -- ^ Versions
   }
   deriving (Show,Eq)


data VersionNeededAuxiliary = VersionNeededAuxiliary
   { vnaHash   :: Word32   -- ^ Hash value of dependency name
   , vnaFlags  :: Word16   -- ^ Dependency specific information
   , vnaOther  :: Word16   -- ^ Unused
   , vnaName   :: Text     -- ^ Dependency name
   }
   deriving (Show,Eq)


-- | Get a linked list of a
-- 'next' returns the next address
getList :: Integral b => Get a -> (a -> b) -> b -> LBS.ByteString -> [a]
getList get next current bs = case next e of
      0           -> [e]
      nextOffset  -> e : getList get next (current + nextOffset) bs
   where
      -- read current entity
      e = runGet get $ LBS.drop (fromIntegral current) bs

getVersionNeededEntries :: Elf -> Section -> [VersionNeeded]
getVersionNeededEntries elf sec =
      case sectionType sec of
         SectionTypeGNU_verneed -> vns
         _                      -> error "Invalid section type"
   where
      -- associated strings section
      stringSec  = getSectionByIndex elf (sectionLink sec)
      -- get a string from the string section by index
      getStr idx = do
         s   <- stringSec
         str <- extractStringFromSection elf s (fromIntegral idx)
         return str
      -- unsafe version
      getStr_    = fromJust . getStr
      -- content of the section
      bs = getSectionContentLBS elf sec
      -- list of RawVersionNeeded
      raws = getList (getRawVersionNeeded (elfPreHeader elf)) rvnNext 0 bs
      -- create VersionNeededAuxiliary from RawVersionNeededAuxiliary
      makeVNA e = VersionNeededAuxiliary
            (rvnaHash e)
            (rvnaFlags e)
            (rvnaOther e)
            (getStr_ $ rvnaName e)
      -- create VersionNeeded from RawVersionNeeded
      makeVN e = VersionNeeded 
            (rvnVersion e) 
            (getStr_ (rvnFileName e)) 
            (fmap makeVNA auxs)
         where
            auxs = getList (getRawVersionNeededAuxiliary (elfPreHeader elf)) rvnaNext 0 tableBS
            tableBS = LBS.drop (fromIntegral $ rvnAuxTable e) bs
      -- list of VersionNeeded
      vns = fmap makeVN raws

-- | Find section with name
findSectionByName :: Elf -> Text -> Maybe Section
findSectionByName elf name = Vector.find p (elfSections elf)
   where
      p x   = getSectionName elf x == Just name

-- | Fields are reused depending on the section types. This type gives a meaningful section type
data FullSectionType
   = SectionTypeRelocation
      { relocSectionHasAddend     :: Bool     -- ^ Indicate whether addends are present
      , relocSectionSymbolTable   :: Section  -- ^ Section containing symbols to relocate
      , relocSectionTargetSection :: Section  -- ^ Section to modify
      }
   | BasicSectionType SectionType
   deriving (Show)

extractZCATable :: Elf -> Section -> ZCATable
extractZCATable elf s = getZCATable (LBS.toStrict bs)
   where
      -- raw section
      bs = getSectionContentLBS elf s

getFullSectionType :: Elf -> Section -> FullSectionType
getFullSectionType elf sec =
   let getSec i = fromJust (getSectionByIndex elf i) in
   case sectionType sec of
      SectionTypeREL    -> SectionTypeRelocation False
                              (getSec $ sectionInfo sec)
                              (getSec $ sectionLink sec)
      SectionTypeRELA   -> SectionTypeRelocation True
                              (getSec $ sectionInfo sec)
                              (getSec $ sectionLink sec)
      t                 -> BasicSectionType t



-- | Entry in the .dynamic section
data DynamicEntry
   = DynEntryRaw RawDynamicEntry                      -- ^ Raw entry
   | DynEntryNone                                     -- ^ Empty entry
   | DynEntryFlags DynamicEntryFlags                  -- ^ Flags
   | DynEntryStateFlags DynamicStateFlags             -- ^ State flags
   | DynEntryPositionalFlags DynamicPositionalFlags   -- ^ Positional flags
   | DynEntryFeatureSelection DynamicFeatures         -- ^ Feature selection
   | DynEntryNeededLibrary Text                       -- ^ Needed library name
   | DynEntryStringTableAddress Word64                -- ^ String table address
   | DynEntryStringTableSize Word64                   -- ^ String table size
   | DynEntrySymbolTableAddress Word64                -- ^ Symbol table address
   | DynEntrySymbolEntrySize Word64                   -- ^ Symbol entry size
   | DynEntryInitFunctionAddress Word64               -- ^ Init function address
   | DynEntryFiniFunctionAddress Word64               -- ^ Fini function address
   | DynEntryInitFunctionArrayAddress Word64          -- ^ Init function array address
   | DynEntryFiniFunctionArrayAddress Word64          -- ^ Fini function array address
   | DynEntryInitFunctionArraySize Word64             -- ^ Init function array size
   | DynEntryFiniFunctionArraySize Word64             -- ^ Fini function array size
   | DynEntrySymbolHashTableAddress Word64            -- ^ Symbol hash table address
   | DynEntryGNUSymbolHashTableAddress Word64         -- ^ GNU symbol hash table address
   | DynEntryPLTRelocAddress Word64                   -- ^ Address of the PLT relocations
   | DynEntryPLTGOTAddress Word64                     -- ^ Address of the global offset table (GOT) for the PLT relocations
   | DynEntryPLTRelocSize Word64                      -- ^ Size in bytes of PLT relocations
   | DynEntryRelocaAddress Word64                     -- ^ Address of relocations with addend
   | DynEntryRelocaSize Word64                        -- ^ Size in bytes of relocations with addend
   | DynEntryRelocaEntrySize Word64                   -- ^ Size in bytes of a relocation with addend entry
   deriving (Show,Eq)

-- | Convert raw dynamic entry into a known entry
toDynamicEntry :: Elf -> Section -> RawDynamicEntry -> DynamicEntry
toDynamicEntry elf sec raw =
   let 
      -- associated strings section
      stringSec  = getSectionByIndex elf (sectionLink sec)
      -- get a string from the string section by index
      getStr idx = do
         s   <- stringSec
         str <- extractStringFromSection elf s (fromIntegral idx)
         return str
      -- unsafe version
      getStr_    = fromJust . getStr
      -- raw entry value
      v          = rawDynValue raw
   in
   case rawDynType raw of
      DynTypeNone                      -> DynEntryNone
      DynTypeFlags                     -> DynEntryFlags (BitSet.fromBits v)
      DynTypeStateFlags                -> DynEntryStateFlags (BitSet.fromBits v)
      DynTypePositionalFlags           -> DynEntryPositionalFlags (BitSet.fromBits v)
      DynTypeFeatureSelection          -> DynEntryFeatureSelection (BitSet.fromBits v)
      DynTypeNeededLibraryName         -> DynEntryNeededLibrary (getStr_ v)
      DynTypeStringTableAddress        -> DynEntryStringTableAddress v
      DynTypeStringTableSize           -> DynEntryStringTableSize v
      DynTypeSymbolTableAddress        -> DynEntrySymbolTableAddress v
      DynTypeSymbolEntrySize           -> DynEntrySymbolEntrySize v
      DynTypeInitFunctionAddress       -> DynEntryInitFunctionAddress v
      DynTypeFiniFunctionAddress       -> DynEntryFiniFunctionAddress v
      DynTypeInitFunctionArrayAddress  -> DynEntryInitFunctionArrayAddress v
      DynTypeFiniFunctionArrayAddress  -> DynEntryFiniFunctionArrayAddress v
      DynTypeInitFunctionArraySize     -> DynEntryInitFunctionArraySize v
      DynTypeFiniFunctionArraySize     -> DynEntryFiniFunctionArraySize v
      DynTypeSymbolHashTableAddress    -> DynEntrySymbolHashTableAddress v
      DynTypeGNUHashTableAddress       -> DynEntryGNUSymbolHashTableAddress v
      DynTypePLTRelocAddress           -> DynEntryPLTRelocAddress v
      DynTypePLTGOTAddress             -> DynEntryPLTGOTAddress v
      DynTypePLTRelocSize              -> DynEntryPLTRelocSize v
      DynTypeRelocaAddress             -> DynEntryRelocaAddress v
      DynTypeRelocaSize                -> DynEntryRelocaSize v
      DynTypeRelocaEntrySize           -> DynEntryRelocaEntrySize v
      _                                -> DynEntryRaw raw

getDynamicEntry :: Elf -> Section -> Get DynamicEntry
getDynamicEntry elf sec = toDynamicEntry elf sec <$> getRawDynamicEntry pre
   where
      pre = elfPreHeader elf
