{-# LANGUAGE LambdaCase #-}

-- | ELF binary format
module ViperVM.Format.Elf
   ( Elf (..)
   , parseElf
   , readElf
     -- * Sections
   , getSectionByIndex
   , getSectionContentBuffer
   , getEntriesWithAlignment
   , getEntriesAndOffsetWithAlignment
   , getEntryTableFromSection
   , getEntryListFromSection
   , findSectionByName
   , FullSectionType (..)
   , getFullSectionType
     -- ** Strings sections
   , getStringsFromSection
   , getStringFromSection
     -- ** Section names
   , getSectionNameByIndex
   , getSectionNamesSection
   , getSectionName
   , getSectionNames
     -- ** Symbols sections
   , getSymbolsFromSection
     -- ** Relocation sections
   , getRelocationEntriesFromSection
     -- ** Dynamic sections
   , DynamicEntry (..)
   , getDynamicEntry
   , getDynamicEntriesFromSection
     -- ** Version sections
   , VersionNeeded (..)
   , VersionNeededAuxiliary (..)
   , getVersionNeededEntriesFromSection
     -- * Notes sections
   , Note (..)
   , getNoteEntriesFromSection
     -- * Debug sections
   , getDebugInfoFromSection
   , getDebugTypeFromSection
   , getDebugAbbrevFromSection
     -- * Intel specific sections
   , getZCATableFromSection
   )
where

import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad (forM)
import Control.Arrow (second)
import Data.Maybe (fromJust)

import qualified ViperVM.Format.Text as Text
import ViperVM.Format.Text (Text)
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Get
import qualified ViperVM.Format.Binary.BitSet as BitSet

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Section
import ViperVM.Format.Elf.Segment
import ViperVM.Format.Elf.Symbol
import ViperVM.Format.Elf.Relocation
import ViperVM.Format.Elf.Dynamic
import ViperVM.Format.Elf.Version
import ViperVM.Format.Elf.Note
import ViperVM.Format.Elf.Intel

import ViperVM.Format.Dwarf

-- | Structure representing a ELF file
--
-- An ELF file starts with a PreHeader that contains the magic number and that
-- describes how the remaining of the file has to be decoded (e.g. endianness,
-- word size).
--
-- Then comes the Header which mentions the target architecture, the type of
-- ELF file, some flags, etc. It also gives information on where to find
-- section and segment tables in the file.
data Elf = Elf
   { elfPreHeader :: PreHeader      -- ^ Pre-header
   , elfHeader    :: Header         -- ^ Header
   , elfSections  :: Vector Section -- ^ Sections
   , elfSegments  :: Vector Segment -- ^ Segments
   , elfContent   :: Buffer         -- ^ Whole content
   } deriving (Show)

-- | Parse a Buffer to retrieve ELF headers and tables.
parseElf :: Buffer -> Elf
parseElf bs = Elf pre hdr sections segments bs
   where
      pre      = runGetOrFail getPreHeader bs
      hdr      = runGetOrFail (skip 16 >> getHeader pre) bs
      sections = getSectionTable bs hdr pre
      segments = getSegmentTable bs hdr pre

-- | Lazily read an ELF file
readElf :: FilePath -> IO Elf
readElf path = parseElf <$> bufferReadFile path


--------------------------------------------------------------
-- SECTIONS
--------------------------------------------------------------

-- | Get a section by index
getSectionByIndex :: Integral a => Elf -> a -> Maybe Section
getSectionByIndex elf i = 
   elfSections elf Vector.!? fromIntegral i


-- | Returns the content of a section as a buffer
getSectionContentBuffer :: Elf -> Section -> Buffer
getSectionContentBuffer elf s = bufferTake sz (bufferDrop off bs)
   where
      bs  = elfContent elf
      sz  = fromIntegral $ sectionSize s 
      off = fromIntegral $ sectionOffset s 
   
-- | Get a sequence of entries. Each entry is aligned to the given number of
-- bytes. The first entry must be correctly aligned.
getEntriesAndOffsetWithAlignment :: Word -> Get a -> Get [(Word64,a)]
getEntriesAndOffsetWithAlignment alignment getter = rec 0
   where
      rec n = isEmpty >>= \case
         True  -> return []
         False -> do
            (cnt,e) <- countBytes $ alignAfter alignment getter
            es <- rec (fromIntegral cnt)
            return ((n,e):es)

-- | Get a sequence of aligned entries with their offset
getEntriesWithAlignment :: Word -> Get a -> Get [a]
getEntriesWithAlignment alignment getter = 
      fmap snd <$> getEntriesAndOffsetWithAlignment alignment getter
         
-- | Return a sequence of entries from a section. The entry size is given by
-- the sectionEntrySize field
getEntryTableFromSection :: Elf -> Section -> Get a -> [a]
getEntryTableFromSection elf sec getter = runGetOrFail (forM [1..cnt] (const getter)) bs
   where
      -- content of the section
      bs = getSectionContentBuffer elf sec
      -- size of a single entry
      size = case fromIntegral (sectionEntrySize sec) of
         0 -> error "Invalid table section: entry size is null"
         x -> x
      -- size of the section
      secsize = fromIntegral (sectionSize sec)
      -- number of entries
      cnt = if secsize /= 0
         then secsize `div` size
         else (0 :: Word64)


-- | Get a linked list of entries
-- 'next' returns the next address
getEntryList :: Integral b => Get a -> (a -> b) -> b -> Buffer -> [a]
getEntryList get next current bs = case next e of
      0           -> [e]
      nextOffset  -> e : getEntryList get next (current + nextOffset) bs
   where
      -- read current entity
      e = runGetOrFail get $ bufferDrop (fromIntegral current) bs

-- | Get a linked list of entries from a section
-- 'next' returns the next address
getEntryListFromSection :: Integral b => Elf -> Section -> Get a -> (a -> b) -> [a]
getEntryListFromSection elf sec get next = 
   getEntryList get next 0 (getSectionContentBuffer elf sec)

-- | Fields are reused depending on the section types. This type gives a meaningful section type
data FullSectionType
   = SectionTypeRelocation
      { relocSectionHasAddend     :: Bool     -- ^ Indicate whether addends are present
      , relocSectionSymbolTable   :: Section  -- ^ Section containing symbols to relocate
      , relocSectionTargetSection :: Section  -- ^ Section to modify
      }
   | BasicSectionType SectionType
   deriving (Show)

-- | Convert a raw section into a more complete section data type
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



--------------------------------------------------------------
-- String sections
--------------------------------------------------------------


-- | Extract strings from a strings section
getStringsFromSection :: Elf -> Section -> [(Word64,Text)]
getStringsFromSection elf sec = 
      case sectionType sec of
         SectionTypeSTRTAB -> fmap f (runGetOrFail getter bs)
         _                 -> error "Invalid section type"
   where
      -- section content
      bs = getSectionContentBuffer elf sec
      -- getter for a bytestring ending with NUL and its offset
      getter = getEntriesAndOffsetWithAlignment 1 getBufferNul
      -- convert a bytestring into text
      f = second Text.decodeUtf8



getStringFromSection :: Elf -> Section -> SectionIndex -> Maybe Text
getStringFromSection elf sec idx = res
   where
      -- Check that section type is valid and index is within range
      res = case (sectionSize sec, sectionType sec) of
         (sz,SectionTypeSTRTAB)
            | sz > fromIntegral idx  -> Just (extractStr sec)
         _                           -> Nothing

      -- extract the string
      extractStr s = Text.decodeUtf8
         $ bufferTakeWhile (/=0)
         $ bufferDrop (fromIntegral idx)
         $ getSectionContentBuffer elf s

--------------------------------------------------------------
-- Section names
--------------------------------------------------------------

-- Names of the sections are stored in a Strings section. The ELF header
-- contains the index of this section.

-- | Return the section containing section names (if any)
getSectionNamesSection :: Elf -> Maybe Section
getSectionNamesSection elf = do
   -- Find the section containing section names
   let secIdx = headerSectionNameIndex (elfHeader elf)
   getSectionByIndex elf secIdx


-- | Return the name of a section from its index
getSectionNameByIndex :: Elf -> SectionIndex -> Maybe Text
getSectionNameByIndex elf idx = do
   -- find the section containing section names
   sec <- getSectionNamesSection elf
   -- extract section name for the index idx
   getStringFromSection elf sec idx


-- | Return the name of a section
getSectionName :: Elf -> Section -> Maybe Text
getSectionName elf = getSectionNameByIndex elf . sectionNameIndex


-- | Return all the section names
getSectionNames :: Elf -> Vector (Section, Maybe Text)
getSectionNames elf = fmap f (elfSections elf)
   where
      f x = (x, getSectionName elf x)

-- | Find section with name
findSectionByName :: Elf -> Text -> Maybe Section
findSectionByName elf name = Vector.find p (elfSections elf)
   where
      p x   = getSectionName elf x == Just name


--------------------------------------------------------------
-- Symbols sections
--------------------------------------------------------------

-- | Get symbols from a section
getSymbolsFromSection :: Elf -> Section -> [SymbolEntry]
getSymbolsFromSection elf sec =
      case sectionType sec of
         SectionTypeSYMTAB -> es
         SectionTypeDYNSYM -> es
         _                 -> error "Invalid section type"
   where
      -- getter for a symbol entry
      getter = getSymbolEntry (elfPreHeader elf)
      -- get table of entries
      es = getEntryTableFromSection elf sec getter

--------------------------------------------------------------
-- Relocations sections
--------------------------------------------------------------

-- | Get relocation entries from a section
getRelocationEntriesFromSection :: Elf -> Section -> [RelocationEntry]
getRelocationEntriesFromSection elf sec = 
      case sectionType sec of
         SectionTypeREL    -> f rel
         SectionTypeRELA   -> f rela
         _                 -> error "Invalid section type"
   where
      -- getter for a relocation entry
      rel  = getRelocationEntry (elfPreHeader elf) (elfHeader elf) False
      rela = getRelocationEntry (elfPreHeader elf) (elfHeader elf) True
      -- get table of entries
      f  = getEntryTableFromSection elf sec


--------------------------------------------------------------
-- Dynamic sections
--------------------------------------------------------------

-- | Dynamic entries
getDynamicEntriesFromSection :: Elf -> Section -> [DynamicEntry]
getDynamicEntriesFromSection elf sec =
      case sectionType sec of
         SectionTypeDYNAMIC -> es
         _                  -> error "Invalid section type"
   where
      -- getter for a dynamic entry
      getter = getDynamicEntry elf sec
      -- get table of entries
      es = getEntryTableFromSection elf sec getter

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
         str <- getStringFromSection elf s (fromIntegral idx)
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

-- | Getter for a single dynamic entry
getDynamicEntry :: Elf -> Section -> Get DynamicEntry
getDynamicEntry elf sec = toDynamicEntry elf sec <$> getRawDynamicEntry pre
   where
      pre = elfPreHeader elf

--------------------------------------------------------------
-- Version sections
--------------------------------------------------------------

-- | Version needed
data VersionNeeded = VersionNeeded
   { vnVersion       :: VersionNeededVersion       -- ^ Version of structure
   , vnFileName      :: Text                       -- ^ Dependency file name
   , vnEntries       :: [VersionNeededAuxiliary]   -- ^ Versions
   }
   deriving (Show,Eq)

-- | Version needed auxiliary
data VersionNeededAuxiliary = VersionNeededAuxiliary
   { vnaHash   :: Word32   -- ^ Hash value of dependency name
   , vnaFlags  :: Word16   -- ^ Dependency specific information
   , vnaOther  :: Word16   -- ^ Unused
   , vnaName   :: Text     -- ^ Dependency name
   }
   deriving (Show,Eq)

-- | Get version needed entries from a section
getVersionNeededEntriesFromSection :: Elf -> Section -> [VersionNeeded]
getVersionNeededEntriesFromSection elf sec =
      case sectionType sec of
         SectionTypeGNU_verneed -> vns
         _                      -> error "Invalid section type"
   where
      -- associated strings section
      stringSec  = getSectionByIndex elf (sectionLink sec)
      -- get a string from the string section by index
      getStr idx = do
         s   <- stringSec
         str <- getStringFromSection elf s (fromIntegral idx)
         return str
      -- unsafe version
      getStr_    = fromJust . getStr
      -- list of RawVersionNeeded
      raws = getEntryListFromSection elf sec 
               (getRawVersionNeeded (elfPreHeader elf)) 
               rvnNext
      -- section content
      bs = getSectionContentBuffer elf sec
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
            auxs    = getEntryList 
                        (getRawVersionNeededAuxiliary (elfPreHeader elf)) 
                        rvnaNext 
                        0 
                        tableBS
            tableBS = bufferDrop (fromIntegral $ rvnAuxTable e) bs
      -- list of VersionNeeded
      vns = fmap makeVN raws

--------------------------------------------------------------
-- Note sections
--------------------------------------------------------------

-- | Note
data Note = Note
   { noteName        :: Text
   , noteDescriptor  :: Buffer
   , noteType        :: Word32
   }
   deriving (Show)


-- | Get note entries
getNoteEntriesFromSection :: Elf -> Section -> [Note]
getNoteEntriesFromSection elf sec = runGetOrFail (getEntriesWithAlignment 4 getter) bs
   where
      -- content of the section
      bs = getSectionContentBuffer elf sec
      -- getter
      getter = do
         raw  <- getRawNote (elfPreHeader elf)
         name <- Text.decodeUtf8 . bufferInit 
                  <$> getBuffer (fromIntegral $ rawnoteNameLength raw)
         desc <- getBuffer (fromIntegral $ rawnoteDescriptorSize raw)
         return (Note name desc (rawnoteType raw))

--------------------------------------------------------------
-- Debug sections
--------------------------------------------------------------

-- | Get debug info
getDebugInfoFromSection :: Elf -> Section -> [DebugInfo]
getDebugInfoFromSection elf sec = runGetOrFail (getEntriesWithAlignment 1 (getDebugInfo endian secAbbrev secStrings)) bs
   where
      endian         = preHeaderEndianness (elfPreHeader elf)
      bs             = getSectionContentBuffer elf sec
      -- section containing abbreviations
      Just secAbbrev = getSectionContentBuffer elf <$> findSectionByName elf (Text.pack ".debug_abbrev")
      -- section containing debug strings
      secStrings     = getSectionContentBuffer elf <$> findSectionByName elf (Text.pack ".debug_str")

-- | Get debug type
getDebugTypeFromSection :: Elf -> Section -> [DebugType]
getDebugTypeFromSection elf sec = runGetOrFail (getEntriesWithAlignment 1 (getDebugType endian)) bs
   where
      endian = preHeaderEndianness (elfPreHeader elf)
      bs = getSectionContentBuffer elf sec

-- | Get debug abbrev
getDebugAbbrevFromSection :: Elf -> Section -> [DebugAbbrevEntry]
getDebugAbbrevFromSection elf sec = runGetOrFail getDebugAbbrevEntries bs
   where
      bs = getSectionContentBuffer elf sec


--------------------------------------------------------------
-- Intel specific sections
--------------------------------------------------------------

-- | Return ZCA table (e.g. optimization report)
getZCATableFromSection :: Elf -> Section -> ZCATable
getZCATableFromSection elf s = getZCATable bs
   where
      -- raw section
      bs = getSectionContentBuffer elf s


