import Text.Printf
import Control.Monad (forM_, (<=<))
import Data.Foldable (traverse_)

import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Platform
import ViperVM.Platform.OpenCL as CL

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig {
      filterOpenCLDevices = fmap (notElem CL.CL_DEVICE_TYPE_CPU) . getDeviceType'
   }

   let memoriesStr x 
         | x <= 1    = printf "%d memory found" x
         | otherwise = printf "%d memories found" x
   putStrLn . memoriesStr . length $ platformMemories pf

   traverse_ (putStrLn <=< memoryInfo) (platformMemories pf)

   putStrLn "OpenCL platforms:"
   forM_ (platformOpenCLPlatforms pf) $ \clPf -> do
      infos <- getPlatformInfos' clPf
      putStrLn $ "  - " ++ (show infos)

   putStrLn "Done."

