-- | Low-level data
module ViperVM.Platform.Memory.Data (
   Data(..), BufferData(..),
   dataCoveringRegion, dataCoveringRegion1D,
   allocateBufferData, allocateBufferDataWithEndianness
) where

import Data.Word (Word64)

import ViperVM.Arch.Common.Endianness
import ViperVM.Arch.Common.Errors

import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.Region
import ViperVM.Platform.Memory.Buffer
import ViperVM.Platform.Memory
import ViperVM.Platform.Types

-- | A data physically stored in memory with the given layout
data Data = Data {
   dataOffset :: Word64,
   dataLayout :: Layout
}

-- | A data associated with its buffer
data BufferData = BufferData {
   bufferDataBuffer :: Buffer,
   bufferDataData :: Data
}

-- | Return the smallest covering shape
dataCoveringShape :: Data -> Shape
dataCoveringShape = layoutCoveringShape . dataLayout

-- | Return the smallest covering region
dataCoveringRegion :: Data -> Region
dataCoveringRegion d = Region (dataOffset d) (dataCoveringShape d)

-- | Return the smallest covering 1D region
dataCoveringRegion1D :: Data -> Region
dataCoveringRegion1D = regionCover1D . dataCoveringRegion


-- | Allocate a data in a newly allocated buffer in memory
allocateBufferData :: Layout -> Memory -> IO (Either AllocError BufferData)
allocateBufferData fm mem = do
   buf <- allocateBuffer (sizeOf fm) mem
   return $ case buf of
      Left err -> Left err
      Right b  -> Right $ BufferData b (Data 0 fm)

-- | Allocate a data in a newly allocated data in memory, passing endianness as a parameter for layout definition
allocateBufferDataWithEndianness :: (Endianness -> Layout) -> Memory -> IO (Either AllocError BufferData)
allocateBufferDataWithEndianness f mem = allocateBufferData (f (memoryEndianness mem)) mem
