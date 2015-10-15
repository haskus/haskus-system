module ViperVM.Format.Elf.Segment
   ( Segment (..)
   , getSegment
   , putSegment
   -- * Internal
   , getSegmentCount
   , getSegmentTable
   )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Binary.Get
import Data.Binary.Put

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.Section

data Segment = Segment
   { segmentType              :: Word32   -- ^ Segment type
   , segmentFlags             :: Word32   -- ^ Segment flags
   , segmentOffset            :: Word64   -- ^ Segment offset in file
   , segmentVirtualAddress    :: Word64   -- ^ Segment virtual address
   , segmentPhysicalAddress   :: Word64   -- ^ Segment physical address
   , segmentSizeInFile        :: Word64   -- ^ Size of the segment in the file
   , segmentSizeInMemory      :: Word64   -- ^ Size of the segment in memory
   , segmentAlignment         :: Word64   -- ^ Segment alignment
   }
   deriving (Show)

getSegment :: PreHeader -> Get Segment
getSegment hdr = do
   let (_,gw32,_,gwN) = getGetters hdr
   case preHeaderWordSize hdr of
      WordSize32 -> do
         typ   <- gw32
         off   <- gwN
         vaddr <- gwN
         paddr <- gwN
         fsz   <- gwN
         msz   <- gwN
         flgs  <- gw32
         algn  <- gwN
         return (Segment typ flgs off vaddr paddr fsz msz algn)
      WordSize64 -> do
         typ   <- gw32
         flgs  <- gw32
         off   <- gwN
         vaddr <- gwN
         paddr <- gwN
         fsz   <- gwN
         msz   <- gwN
         algn  <- gwN
         return (Segment typ flgs off vaddr paddr fsz msz algn)

putSegment :: PreHeader -> Segment -> Put
putSegment hdr s = do
   let (_,pw32,_,pwN) = getPutters hdr

   case preHeaderWordSize hdr of
      WordSize32 -> do
         pw32 (segmentType s)
         pwN  (segmentOffset s)
         pwN  (segmentVirtualAddress s)
         pwN  (segmentPhysicalAddress s)
         pwN  (segmentSizeInFile s)
         pwN  (segmentSizeInMemory s)
         pw32 (segmentFlags s)
         pwN  (segmentAlignment s)
      WordSize64 -> do
         pw32 (segmentType s)
         pw32 (segmentFlags s)
         pwN  (segmentOffset s)
         pwN  (segmentVirtualAddress s)
         pwN  (segmentPhysicalAddress s)
         pwN  (segmentSizeInFile s)
         pwN  (segmentSizeInMemory s)
         pwN  (segmentAlignment s)

-- | If the number of segment doesn't fit int 16 bits, then
-- 'headerSectionEntryCount' is set to 0xffff and the field 'sectionInfo' of
-- section 0 contains the effective value.
getSegmentCount :: ByteString -> Header -> PreHeader -> Word64
getSegmentCount bs hdr pre = 
   if (headerSectionEntryCount hdr /= 0xffff)
      then fromIntegral (headerSegmentEntryCount hdr)
      else fromIntegral (sectionInfo sec)
   where
      -- first section
      sec = getFirstSection bs hdr pre
            
-- | Return the table of segments
getSegmentTable :: ByteString -> Header -> PreHeader -> Vector Segment
getSegmentTable bs h pre = fmap f offs
   where
      f o  = runGet (getSegment pre) (LBS.drop o bs')
      off  = fromIntegral $ headerSegmentTableOffset h
      bs'  = LBS.drop off bs
      sz   = fromIntegral $ headerSegmentEntrySize h
      cnt  = fromIntegral $ getSegmentCount bs h pre
      offs = Vector.fromList [ 0, sz .. (cnt-1) * sz]

