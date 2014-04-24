import Text.Printf
import Control.Monad (forM)
import Control.Monad ((<=<))
import Control.Applicative ((<$>))
import Data.Foldable (traverse_)

import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Types
import ViperVM.Platform.Config
import ViperVM.Platform.Loading
import ViperVM.Platform.Memory.Buffer
import ViperVM.Platform.Memory.FieldMap
import ViperVM.Platform.Memory.Data

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig

   traverse_ (putStrLn <=< memoryInfo) (platformMemories pf)

   putStrLn "\nAllocating data in each memory"
   datas <- forM (platformMemories pf) $ \mem -> do
      let 
         dt = \endian -> Array (Scalar (DoubleField endian)) 128
         f = either (error . ("Allocation error: " ++) . show) id
      
      f <$> allocateDataWithEndianness dt mem

   traverse_ (putStrLn <=< memoryInfo) (platformMemories pf)

   putStrLn "\nReleasing data in each memory"
   traverse_ (releaseBuffer . dataBuffer) datas

   traverse_ (putStrLn <=< memoryInfo) (platformMemories pf)

   putStrLn "Done."

