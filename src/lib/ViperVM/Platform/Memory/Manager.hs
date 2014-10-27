-- | Data allocator
--
-- For now,  we use a very simple policy where a buffer is allocated for each
-- allocated data and released when all of its data are freed
module ViperVM.Platform.Memory.Manager
   ( ManagerConfig(..)
   , Manager(..)
   , DataRef
   , initManager
   , defaultManagerConfig
   , allocateData
   , allocateDataWithEndianness
   , releaseData
   )
where

import ViperVM.Platform.Memory
import ViperVM.Platform.Types(Memory, Data(..))
import ViperVM.Arch.Common.Endianness
import ViperVM.Arch.Common.Errors
import ViperVM.Platform.Memory.Layout


import Data.Word
import qualified ViperVM.STM.TMap as TMap
import ViperVM.STM.TMap (TMap)
import Control.Concurrent.STM
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (traverse_)

type DataRef = Word64

-- | Memory manager
data Manager = Manager 
   { managerMemory   :: Memory
   , managerData     :: TMap DataRef Data
   , managerLastRef  :: TVar DataRef
   }

-- | Memory manager configuration
data ManagerConfig = ManagerConfig

-- | Default manager configuration
defaultManagerConfig :: ManagerConfig
defaultManagerConfig = ManagerConfig

-- | Initialize a memory manager
initManager :: ManagerConfig -> Memory -> IO Manager
initManager _ mem = Manager mem 
   <$> atomically (TMap.empty)
   <*> newTVarIO 0

-- | Store a buffer data and return a new data reference
storeData :: Manager -> Data -> IO DataRef
storeData m d = atomically $ do
   let 
      lst = managerLastRef m
      ds = managerData m
      getRef r = do
         present <- r `TMap.member` ds
         if present
            then getRef (r+1) 
            else return r
   lastRef <- readTVar lst
   ref <- getRef (lastRef + 1)
   TMap.insert ref d ds
   writeTVar lst ref
   return ref


-- | Allocate a data in a newly allocated buffer in memory
allocateData :: Layout -> Manager -> IO (Either AllocError DataRef)
allocateData fm m = do
   let mem = managerMemory m
   buf <- memoryBufferAllocate (sizeOf fm) mem
   case buf of
      Left err -> return (Left err)
      Right b  -> Right <$> storeData m (Data b 0 fm)

-- | Allocate a data in a newly allocated data in memory, passing endianness as a parameter for layout definition
allocateDataWithEndianness :: (Endianness -> Layout) -> Manager -> IO (Either AllocError DataRef)
allocateDataWithEndianness f m = allocateData (f (memoryEndianness mem)) m
   where mem = managerMemory m


-- | Release a data
releaseData :: Manager -> DataRef -> IO ()
releaseData m ref = do
   d' <- atomically $ do
      let ds = managerData m
      d <- TMap.lookup ref ds
      TMap.delete ref ds
      return d

   traverse_ (memoryBufferRelease . dataBuffer) d'
