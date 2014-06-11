import Control.Monad (forM)
import Control.Monad ((<=<))
import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Data.Foldable (traverse_)

import ViperVM.Platform.Host
import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Config
import ViperVM.Platform.Loading
import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.Manager

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig

   let extractMem xs x = return (x:xs)
   mems <- reverse <$> (atomically $ foldMemories pf [] extractMem)

   traverse_ (putStrLn <=< memoryInfo) mems

   putStrLn "\nCreate basic memory manager for each memory"
   mgrs <- forM mems (initManager defaultManagerConfig)

   putStrLn "\nAllocating data in each memory"
   datas <- forM mgrs $ \mgr -> do
      let 
         dt = \endian -> Array (Scalar (DoubleField endian)) 128
         f = either (error . ("Allocation error: " ++) . show) id
      
      f <$> allocateDataWithEndianness dt mgr

   traverse_ (putStrLn <=< memoryInfo) mems

   putStrLn "\nReleasing data in each memory"
   traverse_ (uncurry releaseData) (mgrs `zip` datas)

   traverse_ (putStrLn <=< memoryInfo) mems

   putStrLn "Done."

