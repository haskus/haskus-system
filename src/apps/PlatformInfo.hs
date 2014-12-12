import Text.Printf
import Data.Foldable (traverse_)
--import Data.List (intersperse)

import ViperVM.Platform.Host
import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Loading
import ViperVM.Platform.Config

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   host <- loadPlatform defaultConfig {
            enableOpenCLCPUs = True
         }

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


   mems <- allMemoriesFromHostIO host

   putStrLn . memoriesStr . length $ mems
   traverse_ (showInfo . memoryInfo) mems

   procs <- allProcessorsFromHostIO host

   putStrLn . procsStr $ length procs
   traverse_ (showInfo . procInfo) procs

   nets <- allNetworksFromHostIO host

   putStrLn . netsStr $ length nets
   traverse_ (showInfo . networkInfo) nets

   --putStrLn "Links"
   --traverse_ (\m -> linkTo m =<< atomically (memoryNeighbors m)) (platformMemories host)
