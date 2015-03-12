-- | Graphic card management
module ViperVM.Arch.Linux.Graphics.Card
   ( Card(..)
   , Capability(..)
   , getCard
   , cardCapability
   , cardHasSupportFor
   , cardConnectors
   , cardConnectorFromID
   , cardEncoders
   , cardEncoderFromID
   , cardControllers
   , cardControllerFromID
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.Capability
import ViperVM.Arch.Linux.Graphics.LowLevel.Connector
import ViperVM.Arch.Linux.Graphics.LowLevel.Controller
import ViperVM.Arch.Linux.Graphics.LowLevel.Encoder
import ViperVM.Arch.Linux.Graphics.LowLevel.Card
import ViperVM.Arch.Linux.ErrorCode

import Control.Applicative ((<$>))
import Data.Traversable (traverse)


-- | Indicate if a capability is supported
cardHasSupportFor :: Card -> Capability -> SysRet Bool
cardHasSupportFor card cap = fmap (/= 0) <$> cardCapability card cap

-- | Internal function to retrive card entities from their identifiers
cardEntities :: (Card -> [a]) -> (Card -> a -> IO (Either x b)) -> Card -> IO [b]
cardEntities getIDs getEntityFromID card = do
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      ids = getIDs card
   
   xs <- traverse (getEntityFromID card) ids
   return (foldr f [] xs)

-- | Get connectors (discard errors)
cardConnectors :: Card -> IO [Connector]
cardConnectors = cardEntities cardConnectorIDs cardConnectorFromID

-- | Get encoders (discard errors)
cardEncoders :: Card -> IO [Encoder]
cardEncoders = cardEntities cardEncoderIDs cardEncoderFromID

-- | Get controllers (discard errors)
cardControllers :: Card -> IO [Controller]
cardControllers = cardEntities cardControllerIDs cardControllerFromID
