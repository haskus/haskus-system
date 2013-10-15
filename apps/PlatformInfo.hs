import Text.Printf
import Data.Traversable

import ViperVM.Platform.OpenCL

main :: IO ()
main = do
   let libOpenCL = "libOpenCL.so"

   putStrLn ("Loading OpenCL: " ++ libOpenCL)
   cllib <- loadOpenCL libOpenCL

   platforms <- getPlatforms cllib
   putStrLn $ printf "We found %d OpenCL platform(s):" (length platforms)

   infos <- traverse (getPlatformInfos' cllib) platforms
   mapM_ (putStrLn . show) infos

   putStrLn "Done."

