import Text.Printf
import Control.Monad ((<=<))
import Data.Foldable (traverse_)

import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Types
import ViperVM.Platform.Loading
import ViperVM.Platform.Config
import ViperVM.Arch.OpenCL.All as CL

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig {
      filterOpenCLDevices = fmap (notElem CL.CL_DEVICE_TYPE_CPU) . getDeviceType'
   }

   let showInfo x = putStrLn $ "  - " ++ x

   let memoriesStr x 
         | x <= 1    = printf "%d memory found" x
         | otherwise = printf "%d memories found" x
       procsStr x 
         | x <= 1    = printf "%d processor found" x
         | otherwise = printf "%d processors found" x
       netsStr x 
         | x <= 1    = printf "%d network found" x
         | otherwise = printf "%d networks found" x


   putStrLn . memoriesStr . length $ platformMemories pf
   traverse_ (showInfo <=< memoryInfo) (platformMemories pf)

   putStrLn . procsStr . length $ platformProcs pf
   traverse_ (showInfo <=< procInfo) (platformProcs pf)

   putStrLn . netsStr . length $ platformNetworks pf
   traverse_ (showInfo <=< networkInfo) (platformNetworks pf)
