-- | Graphic card management
module ViperVM.Arch.Linux.Graphics.Card
   ( Card(..)
   , Capability(..)
   , getCard
   , cardCapability
   , cardHasSupportFor
   , cardConnectors
   , cardConnectorFromID
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.Capability
import ViperVM.Arch.Linux.Graphics.LowLevel.Connector
import ViperVM.Arch.Linux.Graphics.LowLevel.Card
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.ErrorCode

import Data.Word
import Control.Applicative ((<$>))
import Data.Traversable (traverse)


-- | Get card capability
cardCapability :: Card -> Capability -> SysRet Word64
cardCapability card cap = withCard card getCapability cap

-- | Indicate if a capability is supported
cardHasSupportFor :: Card -> Capability -> SysRet Bool
cardHasSupportFor card cap = fmap (/= 0) <$> cardCapability card cap

-- | Get connector
cardConnectorFromID :: Card -> ConnectorID -> SysRet Connector
cardConnectorFromID card = withCard card getConnector

-- | Get connectors (discard errors)
cardConnectors :: Card -> IO [Connector]
cardConnectors card = do
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      ids = cardConnectorIDs card
   
   xs <- traverse (cardConnectorFromID card) ids
   return (foldr f [] xs)
