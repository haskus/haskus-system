module ViperVM.Platform.Network (
   networkMemories
) where

import ViperVM.Platform.Types

-- | Retrieve memories interconnected by the network
networkMemories :: Network -> [Memory]
networkMemories net = case net of
   p@(PPPLink {}) -> [pppLinkSource p, pppLinkTarget p]
