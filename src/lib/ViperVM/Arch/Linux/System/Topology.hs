-- | Processor/memory topology
module ViperVM.Arch.Linux.System.Topology
   ( CPUMap(..)
   , parseMemInfo
   , readMemInfo
   , parseCPUMap
   , readCPUMap
   , member
   , Node(..)
   , NUMA(..)
   , loadNUMA
   , nodeMemoryStatus
   , nodeCPUs
   )
where

import Data.Attoparsec.ByteString.Char8
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import System.Directory
import Data.List (isPrefixOf,stripPrefix)
import Control.Monad (void,forM)
import Control.Applicative ((<|>))
import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS
import Data.Bits
import Data.Maybe (fromJust,mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V

-- | A CPUMap is a set of CPU identifiers
--
-- TODO: replace Vector of Word32 with a variable length bitset
data CPUMap = CPUMap (V.Vector Word32) deriving (Show)

-- | Read cpumap files
readMemInfo :: FilePath -> IO (Map Text Word64)
readMemInfo = fmap parseMemInfo . BS.readFile

-- | Read meminfo files
parseMemInfo :: ByteString -> Map Text Word64
parseMemInfo bs = case parseOnly parseFile bs of
      Right v  -> Map.fromList v
      Left err -> error ("meminfo parsing error: " ++ show err)

   where
      parseFile = parseLine `sepBy` endOfLine
      parseLine = do
         void (string (pack "Node "))
         skipMany1 digit
         void (char ' ')
         lbl <- takeTill (== ':')
         skipMany1 space
         value <- decimal
         kb <- (string (pack " kB") *> return (*1024)) <|> return id
         return (decodeUtf8 lbl, kb value)


-- | Read cpumap files
readCPUMap :: FilePath -> IO CPUMap
readCPUMap = fmap parseCPUMap . BS.readFile

-- | Parse CPU map files
parseCPUMap :: ByteString -> CPUMap
parseCPUMap bs = case parseOnly parseFile bs of
      Left err -> error ("cpumap parsing error: " ++ show err)
      Right v  -> CPUMap . V.fromList . reverse . dropWhile (==0) $ v

   where
      parseFile = hexadecimal `sepBy1` (char ',') <* endOfLine

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
data NUMA = NUMA
   { numaNodes :: [Node]
   } deriving (Show)

-- | A NUMA node
data Node = Node
   { nodeId :: Word
   , nodeCPUMap :: CPUMap
   , nodeMemory :: NodeMemory
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

   return (infos Map.! Text.pack "MemTotal", infos Map.! Text.pack "MemFree")

-- | Return a list of CPU numbers from a map in a node
nodeCPUs :: Node -> [Word]
nodeCPUs = fromCPUMap . nodeCPUMap
