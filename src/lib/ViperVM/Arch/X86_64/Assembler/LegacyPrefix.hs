module ViperVM.Arch.X86_64.Assembler.LegacyPrefix 
   ( LegacyPrefix(..)
   , RepeatMode(..)
   , Segment(..)
   , toLegacyPrefix
   , putLegacyPrefixes
   , putLegacyPrefix
   ) where

import Data.Word
import Data.Foldable (traverse_)
import ViperVM.Format.Binary.Put

data LegacyPrefix
   = PrefixOperandSizeOverride
   | PrefixAddressSizeOverride
   | PrefixSegmentOverride Segment
   | PrefixLock
   | PrefixRepeat RepeatMode
   deriving (Eq, Show)

data RepeatMode
   = RepeatEqual 
   | RepeatNotEqual 
   deriving (Eq,Show)

data Segment
   = CS 
   | DS 
   | ES 
   | FS 
   | GS 
   | SS 
   deriving (Eq,Show)


-- | Write a list of legacy prefixes
putLegacyPrefixes :: [LegacyPrefix] -> Put
putLegacyPrefixes = traverse_ putLegacyPrefix

   
-- | Read a legacy prefix if possible
toLegacyPrefix :: Word8 -> LegacyPrefix
toLegacyPrefix x = case x of
   0x66  -> PrefixOperandSizeOverride
   0x67  -> PrefixAddressSizeOverride
   0x2E  -> PrefixSegmentOverride CS
   0x3E  -> PrefixSegmentOverride DS
   0x26  -> PrefixSegmentOverride ES
   0x64  -> PrefixSegmentOverride FS
   0x65  -> PrefixSegmentOverride GS
   0x36  -> PrefixSegmentOverride SS
   0xF0  -> PrefixLock
   0xF3  -> PrefixRepeat RepeatEqual
   0xF2  -> PrefixRepeat RepeatNotEqual
   _     -> error $ "Invalid prefix value: " ++ show x

-- | Write a legacy prefix
putLegacyPrefix :: LegacyPrefix -> Put
putLegacyPrefix p = putWord8 $ case p of
   PrefixOperandSizeOverride  -> 0x66
   PrefixAddressSizeOverride  -> 0x67
   PrefixSegmentOverride CS   -> 0x2E
   PrefixSegmentOverride DS   -> 0x3E
   PrefixSegmentOverride ES   -> 0x26
   PrefixSegmentOverride FS   -> 0x64
   PrefixSegmentOverride GS   -> 0x65
   PrefixSegmentOverride SS   -> 0x36
   PrefixLock                 -> 0xF0
   PrefixRepeat RepeatEqual   -> 0xF3
   PrefixRepeat RepeatNotEqual-> 0xF2
