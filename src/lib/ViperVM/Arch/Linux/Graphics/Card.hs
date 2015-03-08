-- | Graphic card management
module ViperVM.Arch.Linux.Graphics.Card
   ( Card(..)
   , Capability(..)
   , getCard
   , cardCapability
   , cardHasSupportFor
   , cardConnectors
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.Capability
import ViperVM.Arch.Linux.Graphics.LowLevel.Connector
import ViperVM.Arch.Linux.Graphics.LowLevel.Card
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode

import Data.Word
import Control.Applicative ((<$>))
import Data.Traversable (traverse)


-- | Get card capability
cardCapability :: IOCTL -> Card -> Capability -> SysRet Word64
cardCapability ioctl card cap = getCapability ioctl (cardFileDescriptor card) cap

-- | Indicate if a capability is supported
cardHasSupportFor :: IOCTL -> Card -> Capability -> SysRet Bool
cardHasSupportFor ioctl card cap = fmap (/= 0) <$> getCapability ioctl (cardFileDescriptor card) cap

-- | Get connectors (discard errors)
cardConnectors :: IOCTL -> Card -> IO [Connector]
cardConnectors ioctl card = do
   let 
      f (Left _)  xs = xs
      f (Right x) xs = x:xs
      fd = cardFileDescriptor card
      ids = cardConnectorIDs card
   
   xs <- traverse (getConnector ioctl fd) ids
   return (foldr f [] xs)
