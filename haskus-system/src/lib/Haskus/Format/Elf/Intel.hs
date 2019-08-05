-- | Intel specific ELF additions
-- .
-- ZCA tables
-- ==========
-- .
-- ICC (Intel C Compiler) can generate an optimization report with the
-- opt-report-* family of flags. To make the correspondance between the
-- information in the generated file and the produced binary, a debug section
-- called ".debug_opt_report is added to the latter.
--
-- In [1], an old version (1.1) of the structure of this section is described
-- in the context of Cilk. In this version, the name of the section was
-- ".itt_notify_tab". Tables found in this section are called ZCA tables.
--
-- As of ICC 16.0.0, the basic header of the structure is the same and the
-- version is now 2.1. The other fields, however, are different: different
-- sizes, order, etc. As we don't have a specification, their meaning has been
-- inferred by observation and may be subject to errors. Please report them to
-- us!
--
-- [1] https://www.cilkplus.org/sites/default/files/open_specifications/LowOverheadAnnotations.pdf
module Haskus.Format.Elf.Intel
   ( ZCATable (..)
   , ZCATableHeader (..)
   , ZCATableEntry (..)
   , getZCATable
   , getZCATableHeader
   , getZCATableEntry
   , getZCAStringTable
   )
where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Haskus.Utils.Text as Text
import Haskus.Utils.Text (Text, getTextUtf8)
import Haskus.Binary.Buffer
import Haskus.Binary.Get
import Haskus.Number.Word
import Haskus.Utils.Flow (when, forM)


-- | ZCA table
data ZCATable = ZCATable
   { zcaHeader          :: ZCATableHeader
   , zcaEntries         :: [ZCATableEntry]
   }
   deriving (Show)

-- | ZCA table header
data ZCATableHeader = ZCATableHeader
   { zcaVersionMajor    :: Word8    -- ^ Major version number
   , zcaVersionMinor    :: Word8    -- ^ Minor version number
   , zcaEntryOffset     :: Word16   -- ^ Offset of the entry table
   , zcaEntryCount      :: Word32   -- ^ Count of entries that follow
   , zcaStringsOffset   :: Word32   -- ^ Offset in bytes to strings table
   , zcaStringsSize     :: Word32   -- ^ Size of string table (bytes)
   , zcaExprsOffset     :: Word32   -- ^ Offset in bytes to expression table
   , zcaExprsSize       :: Word32   -- ^ Size of expression table (bytes)
   , zcaStuff1          :: Word32
   }
   deriving (Show)

-- | ZCA table name
zcaMagic :: Text
zcaMagic = Text.pack ".itt_notify_tab"

-- | Getter for a ZcA table header
getZCATableHeader :: Get ZCATableHeader
getZCATableHeader = do
   magic <- getTextUtf8 (fromIntegral (Text.length zcaMagic))
   when (magic /= zcaMagic) $
      error "Not a ZCA table (invalid magic number)"
   -- skip magic NUL terminal byte
   skip 1
   versionmaj <- getWord8
   versionmin <- getWord8

   when ((versionmaj,versionmin) /= (2,1)) $
      error "Unsupported ZCA version"

   -- read table header
   ZCATableHeader versionmaj versionmin
      <$> getWord16le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le

-- | ZCA table entry
data ZCATableEntry = ZCATableEntry
   { zcaIP              :: Word64         -- ^ Instruction pointer on entry
   , zcaNameIndex       :: Word32         -- ^ Offset in bytes into strings table
   , zcaName            :: Text           -- ^ Entry string
   , zcaValueIndex      :: Word32         -- ^ Offset in bytes into expression table
   , zcaValue           :: Buffer         -- ^ Values
   }
   deriving (Show)

-- | Getter for a table entry
getZCATableEntry :: Map Int Text -> Get ZCATableEntry
getZCATableEntry strs = do
   off  <- getWord64le
   nidx <- getWord32le
   eoff <- getWord32le

   let name = strs Map.! fromIntegral nidx

   return (ZCATableEntry off nidx name eoff emptyBuffer)

-- | Getter for table entries
getZCATableEntries :: ZCATableHeader -> Buffer -> [ZCATableEntry]
getZCATableEntries hdr bs = es
   where
      -- extract raw table
      raw =   bufferTake (fromIntegral $ zcaEntryCount hdr * 16)
            $ bufferDrop (fromIntegral $ zcaEntryOffset hdr) bs
      -- get strings
      strs = getZCAStringTable hdr bs
      -- decode entries
      getEntries = forM [1..zcaEntryCount hdr] (const $ getZCATableEntry strs)
      es = runGetOrFail getEntries raw

-- | Get string table
getZCAStringTable :: ZCATableHeader -> Buffer -> Map Int Text
getZCAStringTable hdr bs = Map.fromList (offs `zip` strs)
   where
      -- extract raw table
      raw =   bufferTake (fromIntegral $ zcaStringsSize hdr)
            $ bufferDrop (fromIntegral $ zcaStringsOffset hdr) bs
      -- decode strings
      strs = fmap Text.bufferDecodeUtf8 . bufferSplitOn 0 . bufferInit $ raw
      -- add offsets
      offs = scanl (+) 0 $ fmap (\s -> Text.length s + 1) strs

-- | Get values
getZCAValues :: ZCATableHeader -> Buffer -> [ZCATableEntry] -> [ZCATableEntry]
getZCAValues hdr bs es = values
   where
      -- raw table of values
      raw    =   bufferTake (fromIntegral $ zcaExprsSize hdr)
               $ bufferDrop (fromIntegral $ zcaExprsOffset hdr) bs
      -- offsets
      offs   = fmap (fromIntegral . zcaValueIndex) es
      -- sizes
      szs    = fmap (uncurry (-)) (offs' `zip` offs)
         where offs' = tail offs ++ [fromIntegral $ zcaExprsSize hdr]
      -- values: we drop the first byte of the value (value size)
      update e sz = e { zcaValue = bufferDrop 1
                                 $ bufferTake sz
                                 $ bufferDrop off raw }
         where off = fromIntegral $ zcaValueIndex e

      values = fmap (uncurry update) (es `zip` szs)

-- | Get table
getZCATable :: Buffer -> ZCATable
getZCATable bs = zca
   where
      -- ZCA table header
      hdr  = runGetOrFail getZCATableHeader bs
      -- ZCA table entries
      es   = getZCATableEntries hdr bs
      -- table
      zca = ZCATable hdr (getZCAValues hdr bs es)
