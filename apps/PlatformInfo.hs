import Text.Printf

import ViperVM.Platform.Platform

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

   putStrLn "Done."

