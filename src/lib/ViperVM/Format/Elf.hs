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
   , findSectionWithName
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

getSectionSymbols :: Elf -> Section -> [SymbolEntry]
getSectionSymbols elf sec =
      case sectionType sec of
         SectionTypeSYMTAB -> syms
         _                 -> error "Invalid section type"
   where
      -- section content
      content = extractSectionContent elf sec
      -- size of a single entry
      symsize = fromIntegral (getSymbolEntrySize (elfPreHeader elf))
      -- size of the section
      secsize = fromIntegral (sectionSize sec)
      -- number of entries
      n = if secsize /= 0
         then secsize `div` symsize
         else 0
      -- offsets in the section
      offs = [0, symsize .. (n-1) * symsize]
      -- read symbol entry at specific offset
      rd off = runGet (getSymbolEntry (elfPreHeader elf)) (LBS.drop off content)
      -- read symbols
      syms = fmap rd offs

-- | Find section with name
findSectionWithName :: Elf -> Text -> Maybe Section
findSectionWithName elf name = listToMaybe . fmap fst . filter p $ names
   where
      p x   = snd x == Just name
      names = getSectionNames elf
