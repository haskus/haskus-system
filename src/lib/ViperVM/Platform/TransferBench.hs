module ViperVM.Platform.TransferBench
   ( BenchResult(..)
   , bench
   , benchStr
   , transferBench
   )
where

import ViperVM.Platform.Transfer
import ViperVM.Platform.TransferResult
import ViperVM.Platform.Topology
import ViperVM.Platform.Memory.Region
import ViperVM.Platform.Memory.Buffer

import Text.Printf
import Control.Concurrent.STM
import Criterion.Measurement

data BenchResult
   = BenchFailed
   | BenchSuccess Double
   deriving (Eq,Show)

-- | Bench a transfer over a network
transferBench :: Network -> (Buffer,Region) -> (Buffer,Region) -> IO BenchResult
transferBench net (b1,r1) (b2,r2) = do
   (tr,duration) <- bench $ networkTransferRegionSync net (b1,r1) (b2,r2)

   putStrLn $ printf "Computation time: %0.9f sec\n" duration

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
