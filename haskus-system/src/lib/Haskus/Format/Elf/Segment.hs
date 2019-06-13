{-# LANGUAGE DeriveAnyClass #-}

module Haskus.Format.Elf.Segment
   ( Segment (..)
   , SegmentType (..)
   , SegmentFlag (..)
   , SegmentFlags
   , getSegment
   , putSegment
   -- * Internal
   , getSegmentCount
   , getSegmentTable
   )
where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Get
import Haskus.Format.Binary.Put
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word

import Haskus.Format.Elf.PreHeader
import Haskus.Format.Elf.Header
import Haskus.Format.Elf.Section

data Segment = Segment
   { segmentType              :: SegmentType    -- ^ Segment type
   , segmentFlags             :: SegmentFlags   -- ^ Segment flags
   , segmentOffset            :: Word64         -- ^ Segment offset in file
   , segmentVirtualAddress    :: Word64         -- ^ Segment virtual address
   , segmentPhysicalAddress   :: Word64         -- ^ Segment physical address
   , segmentSizeInFile        :: Word64         -- ^ Size of the segment in the file
   , segmentSizeInMemory      :: Word64         -- ^ Size of the segment in memory
   , segmentAlignment         :: Word64         -- ^ Segment alignment
   }
   deriving (Show)

data SegmentType
   = SegmentTypeNone                   -- ^ Program header table entry unused
   | SegmentTypeLoad                   -- ^ Loadable program segment
   | SegmentTypeDynamic                -- ^ Dynamic linking information
   | SegmentTypeInterpreter            -- ^ Program interpreter
   | SegmentTypeInfo                   -- ^ Auxiliary information
   | SegmentTypeSharedLib              -- ^ Reserved
   | SegmentTypeSegmentHeader          -- ^ Entry for header table itself
   | SegmentTypeTLS                    -- ^ Thread-local storage segment
   | SegmentTypeGNU_EH_Frame           -- ^ GCC .eh_frame_hdr segment
   | SegmentTypeGNU_Stack              -- ^ Indicates stack executability
   | SegmentTypeGNU_ReadOnlyAfterReloc -- ^ Read-only after relocation
   | SegmentTypeSunBSS                 -- ^ Sun Specific segment
   | SegmentTypeSunStack               -- ^ Sun stack segment
   | SegmentTypeUnknown Word32         -- ^ Unknown segment type
   deriving (Show,Eq)

data SegmentFlag
   = SegmentFlagExecutable
   | SegmentFlagWritable
   | SegmentFlagReadable
   deriving (Show,Eq,Enum,BitOffset)

type SegmentFlags = BitSet Word32 SegmentFlag

instance CEnum SegmentType where
   fromCEnum x = case x of
      SegmentTypeNone                   -> 0
      SegmentTypeLoad                   -> 1
      SegmentTypeDynamic                -> 2
      SegmentTypeInterpreter            -> 3
      SegmentTypeInfo                   -> 4
      SegmentTypeSharedLib              -> 5
      SegmentTypeSegmentHeader          -> 6
      SegmentTypeTLS                    -> 7
      SegmentTypeGNU_EH_Frame           -> 0x6474e550
      SegmentTypeGNU_Stack              -> 0x6474e551
      SegmentTypeGNU_ReadOnlyAfterReloc -> 0x6474e552
      SegmentTypeSunBSS                 -> 0x6ffffffa
      SegmentTypeSunStack               -> 0x6ffffffb
      SegmentTypeUnknown w              -> fromIntegral w

   toCEnum x = case x of
      0            -> SegmentTypeNone
      1            -> SegmentTypeLoad
      2            -> SegmentTypeDynamic
      3            -> SegmentTypeInterpreter
      4            -> SegmentTypeInfo
      5            -> SegmentTypeSharedLib
      6            -> SegmentTypeSegmentHeader
      7            -> SegmentTypeTLS
      0x6474e550   -> SegmentTypeGNU_EH_Frame
      0x6474e551   -> SegmentTypeGNU_Stack
      0x6474e552   -> SegmentTypeGNU_ReadOnlyAfterReloc
      0x6ffffffa   -> SegmentTypeSunBSS
      0x6ffffffb   -> SegmentTypeSunStack
      w            -> SegmentTypeUnknown (fromIntegral w)



getSegment :: PreHeader -> Get Segment
getSegment hdr = do
   let (_,_,gw32,_,gwN) = getGetters hdr
   case preHeaderWordSize hdr of
      WordSize32 -> do
         typ   <- toCEnum <$> gw32
         off   <- gwN
         vaddr <- gwN
         paddr <- gwN
         fsz   <- gwN
         msz   <- gwN
         flgs  <- BitSet.fromBits <$> gw32
         algn  <- gwN
         return (Segment typ flgs off vaddr paddr fsz msz algn)
      WordSize64 -> do
         typ   <- toCEnum <$> gw32
         flgs  <- BitSet.fromBits <$> gw32
         off   <- gwN
         vaddr <- gwN
         paddr <- gwN
         fsz   <- gwN
         msz   <- gwN
         algn  <- gwN
         return (Segment typ flgs off vaddr paddr fsz msz algn)

putSegment :: PreHeader -> Segment -> Put
putSegment hdr s = do
   let 
      (_,_,pw32,_,pwN) = getPutters hdr
      typ   = fromCEnum . segmentType $ s
      flags = BitSet.toBits (segmentFlags s)

   case preHeaderWordSize hdr of
      WordSize32 -> do
         pw32 typ
         pwN  (segmentOffset s)
         pwN  (segmentVirtualAddress s)
         pwN  (segmentPhysicalAddress s)
         pwN  (segmentSizeInFile s)
         pwN  (segmentSizeInMemory s)
         pw32 flags
         pwN  (segmentAlignment s)
      WordSize64 -> do
         pw32 typ
         pw32 flags
         pwN  (segmentOffset s)
         pwN  (segmentVirtualAddress s)
         pwN  (segmentPhysicalAddress s)
         pwN  (segmentSizeInFile s)
         pwN  (segmentSizeInMemory s)
         pwN  (segmentAlignment s)

-- | If the number of segment doesn't fit int 16 bits, then
-- 'headerSegmentEntryCount' is set to 0xffff and the field 'sectionInfo' of
-- section 0 contains the effective value.
getSegmentCount :: Buffer -> Header -> PreHeader -> Word64
getSegmentCount bs hdr pre = 
   if (headerSegmentEntryCount hdr /= 0xffff)
      then fromIntegral (headerSegmentEntryCount hdr)
      else fromIntegral (sectionInfo sec)
   where
      -- first section
      sec = getFirstSection bs hdr pre
            
-- | Return the table of segments
getSegmentTable :: Buffer -> Header -> PreHeader -> Vector Segment
getSegmentTable bs h pre = 
      if cnt == 0
         then Vector.empty
         else fmap f offs
   where
      f o  = runGetOrFail (getSegment pre) (bufferDrop o bs')
      off  = fromIntegral $ headerSegmentTableOffset h
      bs'  = bufferDrop off bs
      sz   = fromIntegral $ headerSegmentEntrySize h
      cnt  = fromIntegral $ getSegmentCount bs h pre
      offs = Vector.fromList [ 0, sz .. (cnt-1) * sz]

