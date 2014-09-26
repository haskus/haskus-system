{-# LANGUAGE PatternSynonyms #-}

module ViperVM.Platform.Transfer 
   ( Transfer(..)
   , networkTransferData
   , networkTransferRegion
   , networkTransferRegionAsync
   , networkTransferRegionSync
   )
where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import ViperVM.Platform.Topology
import ViperVM.Platform.TransferResult
import ViperVM.Platform.Memory.Buffer
import ViperVM.Platform.Memory.Data
import ViperVM.Platform.Memory.Region
import ViperVM.Platform.Drivers (transferRegion)

-- | A transfer
data Transfer = Transfer 
   { transferResult :: TMVar TransferResult
   }

-- | Asynchronously transfer a data
networkTransferData :: Network -> BufferData -> BufferData -> IO Transfer
networkTransferData net src dst = do
   let
      MemoryBufferData _ b1 d1 = src
      MemoryBufferData _ b2 d2 = dst

      r1 = dataCoveringRegion d1
      r2 = dataCoveringRegion d2

   networkTransferRegionAsync net (b1,r1) (b2,r2)

-- | Transfer a region
networkTransferRegion :: Bool -> Network -> (Buffer,Region) -> (Buffer,Region) -> IO Transfer
networkTransferRegion sync net (b1,r1) (b2,r2) = do
   let
      srcBufferPeer = bufferPeer b1
      dstBufferPeer = bufferPeer b2

   result <- newEmptyTMVarIO
   
   let f = if sync then void else void . forkIO

   f $ do
      -- perform transfer (synchronous)
      res <- transferRegion 
         (networkPeer net)
         (srcBufferPeer, r1)
         (dstBufferPeer, r2)
      -- set result
      atomically $ putTMVar result res

   return (Transfer result)

-- | Asynchronously transfer a region
networkTransferRegionAsync :: Network -> (Buffer,Region) -> (Buffer,Region) -> IO Transfer
networkTransferRegionAsync = networkTransferRegion False

-- | Synchronously transfer a region
networkTransferRegionSync :: Network -> (Buffer,Region) -> (Buffer,Region) -> IO Transfer
networkTransferRegionSync = networkTransferRegion True
