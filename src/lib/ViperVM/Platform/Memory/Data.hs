-- | Low-level data
module ViperVM.Platform.Memory.Data 
   ( dataCoveringRegion
   , dataCoveringRegion1D
   , allocateData
   , allocateDataWithEndianness
   , releaseData
   )
where

import ViperVM.Platform.Types (Memory(..),Data(..))
import ViperVM.Arch.Common.Errors (AllocError(..))
import ViperVM.Arch.Common.Endianness (Endianness(..))
import ViperVM.Platform.Memory
import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.Region

import Control.Concurrent.STM
import Control.Applicative ((<$>))

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
allocateData :: Layout -> Memory -> IO (Either AllocError Data)
allocateData fm mem = do
   buf <- memoryBufferAllocate (sizeOf fm) mem
   --TODO: associate data with buffer?
   case buf of
      Left err -> return (Left err)
      Right b  -> Right . Data b 0 fm <$> atomically newEmptyTMVar

-- | Allocate a data in a newly allocated data in memory, passing endianness as a parameter for layout definition
allocateDataWithEndianness :: (Endianness -> Layout) -> Memory -> IO (Either AllocError Data)
allocateDataWithEndianness f mem = 
   allocateData (f $ memoryEndianness mem) mem


-- | Release a data
releaseData :: Data -> IO ()
releaseData d = do
   let b = dataBuffer d

   --TODO: disassociate data from buffer?
   --TODO: check if we have to/can release the buffer?
   memoryBufferRelease b
