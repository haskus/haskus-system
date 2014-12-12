import Control.Monad (forM)
import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Data.Foldable (traverse_,forM_)

import ViperVM.Platform.Host
import ViperVM.Platform.Config
import ViperVM.Platform.Loading
import ViperVM.Platform.Memory.Layout
import ViperVM.Platform.Memory.Data
import qualified ViperVM.Platform.Data.Vector as Vector
import ViperVM.Platform.Topology
import ViperVM.Platform.Transfer
import ViperVM.Platform.TransferBench
import qualified ViperVM.STM.TSet as TSet

main :: IO ()
main = do
   putStrLn "Loading Platform..."
   host <- loadPlatform defaultConfig {
            enableOpenCLCPUs = True
         }

   mems <- allMemoriesFromHostIO host

   putStrLn "============================================"
   putStrLn "Testing allocation/release..."

   let
      vsize = 200*1024*1024
   vector <- atomically $ Vector.new (Vector.VectorParameters Vector.FDouble vsize)

   putStrLn "\nAllocating data in each memory"
   datas <- forM mems $ \mem -> do
      let 
         layout endian = Array (Scalar (DoubleField endian)) vsize
         f = either (error . ("Allocation error: " ++) . show) id
      
      d <- f <$> allocateDataWithEndianness layout mem
      atomically $ Vector.attach_ vector Vector.DenseVector d
      return d

   putStrLn "\nReleasing data in each memory"
   traverse_ releaseData datas


   putStrLn "============================================"
   putStrLn "Testing transfers..."

   putStrLn "\nTransferring data between each directly connected memory"
   forM_ mems $ \m1 -> do
      ms <- atomically (TSet.toList =<< memoryNetNeighbors m1)
      forM_ ms $ \(net,m2) -> do
         putStrLn "\n  - Allocating data in 2 memories"
         let 
            dt endian = Array (Scalar (DoubleField endian)) 128
            getOrFail = either (error . ("Allocation error: " ++) . show) id
         d1 <- getOrFail <$> allocateDataWithEndianness dt m1
         d2 <- getOrFail <$> allocateDataWithEndianness dt m2

         putStrLn "  - Transferring between memories..."
         (tr,duration) <- benchStr $ networkTransferData net d1 d2
         putStrLn "  - Waiting for transfer to end..."
         res <- atomically $ takeTMVar (transferResult tr)
         putStrLn ("  - Transfer result: " ++ show res ++ " duration: " ++ duration ++ " secs")

         putStrLn "  - Releasing data"
         releaseData d1
         releaseData d2
   
   putStrLn "Done."

