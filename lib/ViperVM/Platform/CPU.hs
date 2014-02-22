module ViperVM.Platform.CPU where

import Control.Applicative ((<$>))
import System.Directory
import Data.List (isPrefixOf)
import Control.Monad (forM,void)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import Data.Word
import qualified Data.Map as Map
import Data.Map ((!))

data NUMA = NUMA {
   numaNodes :: [Node]
} deriving (Show)

data Node = Node {
   nodeCPUs :: [FilePath],
   nodeMemory :: NodeMemory
} deriving (Show)

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
   meminfo <- readFile path

   infos <- Map.fromList <$> case parse parseFile "" meminfo of
      Left err -> error ("meminfo parsing error: " ++ show err)
      Right v -> return v

   return ((*1024) . read $ infos ! "MemTotal", 
           (*1024) . read $ infos ! "MemFree")

   where
      parseFile = many1 parseLine
      parseLine = do
         void (string "Node ")
         void (many1 digit)
         void (char ' ')
         lbl <- manyTill anyChar (char ':')
         skipMany1 space
         value <- many1 digit
         void (manyTill anyChar (void newline <|> eof))
         return (lbl,value)
