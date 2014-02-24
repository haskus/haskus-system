-- | SysFS (Linux) management module
module ViperVM.Platform.CPU where

import Control.Applicative ((<$>))
import System.Directory
import Data.List (isPrefixOf)
import Control.Monad (forM)
import Data.Word
import Data.Map ((!))

import ViperVM.Platform.Host.SysFS (readMemInfo)

-- | A set of NUMA nodes
data NUMA = NUMA {
   numaNodes :: [Node]
} deriving (Show)

-- | A NUMA node
data Node = Node {
   nodeCPUs :: [FilePath],
   nodeMemory :: NodeMemory
} deriving (Show)

-- | A memory node
newtype NodeMemory = NodeMemory FilePath deriving (Show)

-- | Load platform from sysfs (Linux)
loadNUMA :: FilePath -> IO NUMA
loadNUMA sysfsPath = do
   let nodePath = sysfsPath ++ "/devices/system/node/"

   nDirs <- filter ("node" `isPrefixOf`) <$> getDirectoryContents nodePath

   ndes <- forM nDirs $ \nDir -> do
      contents <- getDirectoryContents (nodePath ++ nDir)
      let
         cpuDirs = filter ("cpu" `isPrefixOf`) contents

      return $ Node cpuDirs (NodeMemory $ nodePath ++ nDir ++ "/meminfo")

   return $ NUMA ndes


-- | Return (total,free) memory for the given node
nodeMemoryStatus :: NodeMemory -> IO (Word64,Word64)
nodeMemoryStatus (NodeMemory path) = do
   infos <- readMemInfo path

   return (infos ! "MemTotal", infos ! "MemFree")
