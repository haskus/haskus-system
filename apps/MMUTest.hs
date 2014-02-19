import Text.Printf
import Control.Monad (forM)

import ViperVM.Platform.Platform
import ViperVM.MMU.DataType
import ViperVM.MMU.Data

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig

   putStrLn "Allocating an array in each memory"
   datas <- forM (platformMemories pf) $ \mem -> do
      let 
         endian = memoryEndianness mem
         dt = Array (Scalar (TDouble endian)) 128
         off = 0
         region = coveringRegionFromDataType dt off
      
      buf <- allocateBufferFromRegion region mem
      case buf of
         Left err -> error ("Allocation error: " ++ show err)
         Right b -> return $ Data dt off b

   putStrLn "Done."

