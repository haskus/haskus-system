import Text.Printf
import Control.Monad (forM_)

import ViperVM.Platform.Platform
import ViperVM.Platform.OpenCL

main :: IO ()
main = do
   let config = PlatformConfig {
         libraryOpenCL = "libOpenCL.so"
       }

   putStrLn "Loading Platform..."
   pf <- loadPlatform config

   let memoriesStr x 
         | x <= 1    = printf "%d memory found" x
         | otherwise = printf "%d memories found" x
   putStrLn . memoriesStr . length $ platformMemories pf

   putStrLn "OpenCL platforms:"
   forM_ (platformOpenCLPlatforms pf) $ \clPf -> do
      infos <- getPlatformInfos' (platformOpenCLLibrary pf) clPf
      putStrLn $ "  - " ++ (show infos)

   putStrLn "Done."

