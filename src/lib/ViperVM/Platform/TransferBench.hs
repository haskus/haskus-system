module ViperVM.Platform.TransferBench
   ( bench
   , benchStr
   , transferBench
   , networkBench
   , networkBenchStore
   )
where

import ViperVM.Platform.Transfer
import ViperVM.Platform.TransferResult
import ViperVM.Platform.Types
   ( Memory
   , Network(..)
   , NetworkBenchResult(..)
   , BenchResult(..)
   , Buffer
   )
import ViperVM.Platform.Memory.Region
import ViperVM.Platform.Memory

import qualified ViperVM.Utils.STM.TMap as TMap
import qualified ViperVM.Utils.STM.TList as TList

import Control.Concurrent.STM
import Data.Traversable (forM)
import Criterion.Measurement

-- | Bench network and store results
networkBenchStore :: Network -> Memory -> Memory -> IO ()
networkBenchStore net m1 m2 = do
   res <- networkBench net m1 m2
   
   atomically $ do
      let benchs = networkBenchs net
      bs <- TMap.lookup (m1,m2) benchs
      case bs of
         Nothing -> do
            s <- TList.singleton res
            TMap.insert (m1,m2) s benchs
         Just rs -> TList.append_ res rs


-- | Bench a link between two memories
--
-- Perform several kind of transfers
networkBench :: Network -> Memory -> Memory -> IO NetworkBenchResult
networkBench net m1 m2 = do
   -- Bench 1D transfers
   let mega n = n * 1024 * 1024 
       bufSizes1D = [32, 128, 1024, 4096, 16*1024, 128*1024, 512*1024,
                     mega 1, mega 8, mega 32, mega 128, mega 512]
                     
   res1D <- forM bufSizes1D $ \size -> do
      -- allocate buffers
      res1 <- memoryBufferAllocate size m1
      res2 <- memoryBufferAllocate size m2
      case (res1,res2) of
         (Left _, Left _)     -> return BenchFailed
         (Left _, Right b)    -> memoryBufferRelease b >> return BenchFailed
         (Right b, Left _)    -> memoryBufferRelease b >> return BenchFailed
         (Right b1, Right b2) -> do
            let r = Region1D 0 size
            res <- transferBench net (b1,r) (b2,r)
            memoryBufferRelease b1
            memoryBufferRelease b2
            return res

   -- Return results
   NetworkBenchResult <$> atomically (TList.fromList $ bufSizes1D `zip` res1D)
                     

-- | Bench a transfer over a network
transferBench :: Network -> (Buffer,Region) -> (Buffer,Region) -> IO BenchResult
transferBench net (b1,r1) (b2,r2) = do
   (tr,duration) <- bench $ networkTransferRegionSync net (b1,r1) (b2,r2)

   res <- atomically $ readTMVar (transferResult tr)
   return $ case res of
      TransferSuccess -> BenchSuccess duration
      TransferError _ -> BenchFailed

-- | Bench an action
bench :: IO a -> IO (a,Double)
bench f = do
   start <- getCPUTime
   res <- f
   end <- getCPUTime

   return (res, end - start) 

-- | Bench an action, return a formatted string for the duration
benchStr :: IO a -> IO (a, String)
benchStr f = do
   (res,t) <- bench f
   return (res, secs t)
