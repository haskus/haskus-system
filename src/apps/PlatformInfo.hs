import Text.Printf
import Data.Foldable (traverse_)
--import Data.List (intersperse)
import Control.Concurrent.STM
import Control.Applicative ((<$>))
import qualified Data.Set as Set

import ViperVM.Platform.Host
import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Loading
import ViperVM.Platform.Config
import ViperVM.Platform.Topology

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig

   let 
      showInfo x = putStrLn $ "  - " ++ x

      memoriesStr x 
         | x <= 1    = printf "%d memory found" x
         | otherwise = printf "%d memories found" x
      procsStr x 
         | x <= 1    = printf "%d processor found" x
         | otherwise = printf "%d processors found" x
      netsStr x 
         | x <= 1    = printf "%d network found" x
         | otherwise = printf "%d networks found" x

      --linkTo m mis = putStrLn $ "  - " ++ show (memoryId m) ++ " -> " ++ concat (intersperse ", " (fmap (show . memoryId) mis))

      extractMem xs x = return (x:xs)

   mems <- reverse <$> (atomically $ foldMemories pf [] extractMem)

   putStrLn . memoriesStr . length $ mems
   traverse_ (showInfo . memoryInfo) mems

   procs <- Set.unions <$> atomically (mapM (readTVar . memoryProcs) mems)

   putStrLn . procsStr . Set.size $ procs
   traverse_ (showInfo . procInfo) procs

   nets <- Set.unions <$> atomically (mapM (readTVar . memoryNetworks) mems)

   putStrLn . netsStr . Set.size $ nets
   traverse_ (showInfo . networkInfo) nets

   --putStrLn "Links"
   --traverse_ (\m -> linkTo m =<< atomically (memoryNeighbors m)) (platformMemories pf)
