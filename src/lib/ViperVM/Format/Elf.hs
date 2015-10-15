{-# LANGUAGE LambdaCase #-}
module ViperVM.Format.Elf
   ( Elf (..)
   , parseElf
   , readElf
   , extractSectionContent
   , extractSectionStrings
   , extractStringFromSection
   , extractSectionNameByIndex
   , getSectionNames
   , getSectionSymbols
   , getRelocationEntries
   , findSectionWithName
   , extractZCATable
   , FullSectionType (..)
   , getFullSectionType
   )
where

import Data.Int
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import qualified Data.Text.Encoding as Text

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Section
import ViperVM.Format.Elf.Symbol
import ViperVM.Format.Elf.Relocation
import ViperVM.Format.Elf.Intel

-- | Structure representing a ELF file
data Elf = Elf
   { elfPreHeader :: PreHeader   -- ^ Pre-header
   , elfHeader    :: Header      -- ^ Header
   , elfSections  :: [Section]   -- ^ Sections
   , elfContent   :: ByteString  -- ^ Whole content
   } deriving (Show)

-- | Parse the ELF format
parseElf :: ByteString -> Elf
parseElf bs = Elf pre hdr sections bs
   where
      pre      = runGet getPreHeader bs
      hdr      = runGet (skip 16 >> getHeader pre) bs
      sections = parseSectionTable bs hdr pre

-- | Read a ELF file
readElf :: FilePath -> IO Elf
readElf path = parseElf <$> LBS.readFile path


extractSectionContent :: Elf -> Section -> ByteString
extractSectionContent elf s = LBS.take sz (LBS.drop off bs)
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
      content = extractSectionContent elf sec
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
         $ extractSectionContent elf s


extractSectionNameByIndex :: Elf -> SectionIndex -> Maybe Text
extractSectionNameByIndex elf idx = extractStringFromSection elf sec idx
   where
      -- Find the section containing section names
      secIdx = headerSectionNameIndex (elfHeader elf)
      sec    = elfSections elf !! fromIntegral secIdx

getSectionNames :: Elf -> [(Section, Maybe Text)]
getSectionNames elf = [ (sec, f sec) | sec <- elfSections elf]
   where
      f = extractSectionNameByIndex elf . sectionNameIndex

getTable :: Elf -> Section -> [ByteString]
getTable elf sec = bss
   where
      -- content of the section
      content = extractSectionContent elf sec
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

-- | Find section with name
findSectionWithName :: Elf -> Text -> Maybe Section
findSectionWithName elf name = listToMaybe . fmap fst . filter p $ names
   where
      p x   = snd x == Just name
      names = getSectionNames elf

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
      bs = extractSectionContent elf s

getFullSectionType :: Elf -> Section -> FullSectionType
getFullSectionType elf sec =
   let getSec i = elfSections elf !! fromIntegral i in
   case sectionType sec of
      SectionTypeREL    -> SectionTypeRelocation False
                              (getSec $ sectionInfo sec)
                              (getSec $ sectionLink sec)
      SectionTypeRELA   -> SectionTypeRelocation True
                              (getSec $ sectionInfo sec)
                              (getSec $ sectionLink sec)
      t                 -> BasicSectionType t


