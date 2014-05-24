{-# LANGUAGE PatternSynonyms #-}

module ViperVM.Platform.Transfer (
   Transfer(..),
   networkTransferData,
   networkTransferRegion
) where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import ViperVM.Platform.Topology
import ViperVM.Platform.TransferResult
import ViperVM.Platform.Memory.Buffer
import ViperVM.Platform.Memory.Data
import ViperVM.Platform.Memory.Region
import ViperVM.Platform.Drivers (transferRegion)

data Transfer = Transfer {
   transferResult :: TMVar TransferResult
}

networkTransferData :: Network -> BufferData -> BufferData -> IO Transfer
networkTransferData net src dst = do
   let
      MemoryBufferData _ b1 d1 = src
      MemoryBufferData _ b2 d2 = dst

      r1 = dataCoveringRegion d1
      r2 = dataCoveringRegion d2

   networkTransferRegion net (b1,r1) (b2,r2)


networkTransferRegion :: Network -> (Buffer,Region) -> (Buffer,Region) -> IO Transfer
networkTransferRegion net (b1,r1) (b2,r2) = do
   let
      srcBufferPeer = bufferPeer b1
      dstBufferPeer = bufferPeer b2

   result <- newEmptyTMVarIO
   
   void $ forkIO $ do
      res <- transferRegion (networkPeer net) (srcBufferPeer,r1) (dstBufferPeer,r2)
      atomically $ putTMVar result res

   return (Transfer result)
