{-# LANGUAGE LambdaCase #-}
module ViperVM.Format.Elf
   ( Elf (..)
   , parseElf
   , readElf
   )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Section

-- | Structure representing a ELF file
data Elf = Elf
   { elfPreHeader :: PreHeader        -- ^ Pre-header informations
   , elfHeader    :: Header      -- ^ Header
   , elfSections  :: [Section]   -- ^ Sections
   } deriving (Show)

-- | Parse the ELF format
parseElf :: ByteString -> Elf
parseElf bs = Elf pre hdr sections
   where
      pre      = runGet getPreHeader bs
      hdr      = runGet (skip 16 >> getHeader pre) bs
      sections = parseSectionTable bs hdr pre

-- | Read a ELF file
readElf :: FilePath -> IO Elf
readElf path = parseElf <$> LBS.readFile path


