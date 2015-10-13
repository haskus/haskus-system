{-# LANGUAGE LambdaCase #-}
module ViperVM.Format.Elf
   ( Elf (..)
   , parseElf
   , readElf
   , extractSectionContent
   , extractSectionStrings
   , extractSectionNameByIndex
   )
where

import Data.Int
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Text (Text)
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


extractSectionNameByIndex :: Elf -> SectionIndex -> Maybe Text
extractSectionNameByIndex elf idx = res
   where
      -- Find the section containing section names
      secIdx = headerSectionNameIndex (elfHeader elf)
      sec    = elfSections elf !! fromIntegral secIdx

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
