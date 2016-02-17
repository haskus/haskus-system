{-# LANGUAGE TupleSections #-}

-- | Platform topology
module ViperVM.Platform.Topology 
   ( isHostMemory
   , memoryNeighbors
   , memoryNetNeighbors
   )
where

import Control.Concurrent.STM
import Data.Traversable (forM)

import qualified ViperVM.Platform.Drivers as Peer
import ViperVM.Platform.Types (Memory(..), Network(..))
import ViperVM.Utils.STM.TSet as TSet


-- | Indicate if a memory is an host memory
isHostMemory :: Memory -> Bool
isHostMemory m = case memoryPeer m of
   Peer.HostMemory {} -> True
   _                  -> False

-- | Memory neighbor memory nodes
memoryNeighbors :: Memory -> STM (TSet Memory)
memoryNeighbors mem = do
   nets <- TSet.toList (memoryNetworks mem)
   TSet.unions =<< traverse (`networkNeighbors` mem) nets

-- | Memory neighbor memory nodes + interconnecting network
memoryNetNeighbors :: Memory -> STM (TSet (Network,Memory))
memoryNetNeighbors mem = do
   nets <- TSet.toList (memoryNetworks mem)
   ss <- forM nets $ \net -> do
      ms <- networkNeighbors net mem
      TSet.map (net,) ms
   TSet.unions ss
