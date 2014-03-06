import Text.Printf
import Control.Monad (forM)
import Control.Monad ((<=<))
import Data.Foldable (traverse_)

import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Platform
import ViperVM.MMU.FieldMap
import ViperVM.MMU.Data

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig

   traverse_ (putStrLn <=< memoryInfo) (platformMemories pf)

   putStrLn "\nAllocating data in each memory"
   datas <- forM (platformMemories pf) $ \mem -> do
      let 
         endian = memoryEndianness mem
         dt = Array (Scalar (DoubleField endian)) 128
         off = 0
      
      buf <- allocateBuffer (sizeOf dt) mem
      case buf of
         Left err -> error ("Allocation error: " ++ show err)
         Right b -> return $ Data dt off b

   traverse_ (putStrLn <=< memoryInfo) (platformMemories pf)

   putStrLn "\nReleasing data in each memory"
   traverse_ (releaseBuffer . dataBuffer) datas

   traverse_ (putStrLn <=< memoryInfo) (platformMemories pf)

   putStrLn "Done."

