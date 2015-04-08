-- | Processor/memory topology
module ViperVM.Arch.Linux.System.Topology
   ( CPUMap(..)
   , readMemInfo
   , readCPUMap
   , member
   , Node(..)
   , NUMA(..)
   , loadNUMA
   , nodeMemoryStatus
   , nodeCPUs
   )
where

import System.Directory
import Data.List (isPrefixOf,stripPrefix)
import Control.Monad (void,forM)
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import Data.Word
import Data.Bits
import Data.Maybe (fromJust,isJust,mapMaybe)
import qualified Data.Map as Map
import qualified Data.Vector as V

-- | A CPUMap is a set of CPU identifiers
--
-- TODO: replace Vector of Word32 with a variable length bitset
data CPUMap = CPUMap (V.Vector Word32) deriving (Show)

-- | Read meminfo files
readMemInfo :: FilePath -> IO (Map.Map String Word64)
readMemInfo path = do
   meminfo <- readFile path

   Map.fromList <$> case parse parseFile "" meminfo of
      Left err -> error ("meminfo parsing error: " ++ show err)
      Right v -> return v

   where
      parseFile = many1 parseLine
      parseLine = do
         void (string "Node ")
         void (many1 digit)
         void (char ' ')
         lbl <- manyTill anyChar (char ':')
         skipMany1 space
         value <- read <$> many1 digit
         kb <- optionMaybe (try(string " kB"))
         void (void newline <|> eof)
         let f x = if isJust kb then x * 1024 else x
         return (lbl, f value)


-- | Read cpumap files
readCPUMap :: FilePath -> IO CPUMap
readCPUMap path = do
   f <- readFile path

   CPUMap . V.fromList . reverse . dropWhile (== 0) <$> case parse parseFile "" f of
      Left err -> error ("cpumap parsing error: " ++ show err)
      Right v -> return v

   where
      parseFile = sepBy1 hexnum (char ',') <* newline <* eof

-- | Check that a CPU belongs to a CPU Map
member :: Word -> CPUMap -> Bool
member idx (CPUMap v) = q < V.length v && testBit (v V.! q) r
   where
      (q,r) = fromIntegral idx `quotRem` 32

-- | Transform a CPUMap into a list of identifiers
fromCPUMap :: CPUMap -> [Word]
fromCPUMap (CPUMap v) = go 0 (V.toList v)
   where
      go _ [] = []
      go n (x:xs) = mapMaybe (f x n) [0..31] ++ go (n+1) xs
      f x n idx = if testBit x idx then Just (n * 32 + fromIntegral idx) else Nothing


-- | A set of NUMA nodes
data NUMA = NUMA {
   numaNodes :: [Node]
} deriving (Show)

-- | A NUMA node
data Node = Node {
   nodeId :: Word,
   nodeCPUMap :: CPUMap,
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
      let nid = read (fromJust $ stripPrefix "node" nDir)
      cpus <- readCPUMap (nodePath ++ nDir ++ "/cpumap")
      return $ Node nid cpus (NodeMemory $ nodePath ++ nDir ++ "/meminfo")

   return $ NUMA ndes


-- | Return (total,free) memory for the given node
nodeMemoryStatus :: NodeMemory -> IO (Word64,Word64)
nodeMemoryStatus (NodeMemory path) = do
   infos <- readMemInfo path

   return (infos Map.! "MemTotal", infos Map.! "MemFree")

-- | Return a list of CPU numbers from a map in a node
nodeCPUs :: Node -> [Word]
nodeCPUs = fromCPUMap . nodeCPUMap
