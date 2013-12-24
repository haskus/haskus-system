module ViperVM.Platform.CPU where

import Control.Applicative ((<$>))
import System.Directory
import Data.List (isPrefixOf)
import Control.Monad (forM)

data Platform = Platform {
   nodes :: [Node]
} deriving (Show)

data Node = Node [FilePath] [FilePath] deriving (Show)

-- | Load platform from sysfs (Linux)
loadPlatform :: FilePath -> IO Platform
loadPlatform sysfsPath = do
   let nodePath = sysfsPath ++ "/devices/system/node/"

   nDirs <- filter ("node" `isPrefixOf`) <$> getDirectoryContents nodePath

   ndes <- forM nDirs $ \nDir -> do
      contents <- getDirectoryContents (nodePath ++ nDir)
      let
         cpuDirs = filter ("cpu" `isPrefixOf`) contents
         memDirs = filter ("memory" `isPrefixOf`) contents
      return $ Node cpuDirs memDirs

   return $ Platform ndes

