{-# LANGUAGE RecordWildCards #-}

-- | Network related functions
module ViperVM.Platform.Network (
   networkMemories, memoryNeighbors
) where

import ViperVM.Platform.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM

-- | Retrieve memories interconnected by the network
networkMemories :: Network -> [Memory]
networkMemories net = case net of
   PPPLink {..} -> [pppLinkSource, pppLinkTarget]

-- | Return memories directly reachable through a network
memoryNeighbors :: Memory -> IO (Map Memory [Network])
memoryNeighbors source = do
   nets <- readTVarIO (memoryNetworks source)
   return $ Map.fromListWith (++) [(mem,[net]) | net <- nets, 
                                      mem <- networkMemories net, 
                                      mem /= source]
