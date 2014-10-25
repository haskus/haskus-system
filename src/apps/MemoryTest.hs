import Control.Monad (forM)
import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Data.Foldable (traverse_,forM_)
import qualified Data.Map as Map

import ViperVM.Platform.Host
import ViperVM.Platform.Config
import ViperVM.Platform.Loading
import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.Manager
import ViperVM.Platform.Topology
import ViperVM.Platform.Transfer
import ViperVM.Platform.TransferBench
import ViperVM.STM.TMap as TMap
import qualified ViperVM.STM.TSet as TSet

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   pf <- loadPlatform defaultConfig {
            enableOpenCLCPUs = True
         }

   let extractMem xs x = return (x:xs)
   mems <- reverse <$> (atomically $ foldMemories pf [] extractMem)

   putStrLn "\nCreate basic memory manager for each memory"
   mgrs <- forM mems (initManager defaultManagerConfig)
   let memManagers = Map.fromList (mems `zip` mgrs)

   putStrLn "============================================"
   putStrLn "Testing allocation/release..."

   putStrLn "\nAllocating data in each memory"
   datas <- forM mgrs $ \mgr -> do
      let 
         dt = \endian -> Array (Scalar (DoubleField endian)) (200*1024*1024)
         f = either (error . ("Allocation error: " ++) . show) id
      
      f <$> allocateDataWithEndianness dt mgr

   putStrLn "\nReleasing data in each memory"
   traverse_ (uncurry releaseData) (mgrs `zip` datas)


   putStrLn "============================================"
   putStrLn "Testing transfers..."

   putStrLn "\nTransferring data between each directly connected memory"
   forM_ mems $ \m1 -> do
      ms <- atomically $ (TSet.toList =<< memoryNetNeighbors m1)
      forM_ ms $ \(net,m2) -> do
         putStrLn "\n  - Allocating data in 2 memories"
         let 
            dt = \endian -> Array (Scalar (DoubleField endian)) 128
            mgr1 = memManagers Map.! m1
            mgr2 = memManagers Map.! m2
            f = either (error . ("Allocation error: " ++) . show) id
         d1 <- allocateDataWithEndianness dt mgr1
         d2 <- allocateDataWithEndianness dt mgr2
         b1 <- atomically $ managerData mgr1 TMap.! f d1
         b2 <- atomically $ managerData mgr2 TMap.! f d2

         putStrLn "  - Transferring between memories..."
         (tr,duration) <- benchStr $ networkTransferData net b1 b2
         putStrLn "  - Waiting for transfer to end..."
         res <- atomically $ takeTMVar (transferResult tr)
         putStrLn ("  - Transfer result: " ++ show res ++ " duration: " ++ duration ++ " secs")

         putStrLn "  - Releasing data"
         releaseData mgr1 (f d1)
         releaseData mgr2 (f d2)
   
   putStrLn "Done."

