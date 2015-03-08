-- | Interface to Linux graphics API
--
-- Linux currently uses KMS/DRM interface
module ViperVM.Arch.Linux.Graphics.Graphics
   ( drmIoctl
   , getConnectorController
   , getEncoderControllers
   , getEncoderConnectors
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.Graphics.Card

import Data.Traversable (traverse)
import Data.Bits (Bits,testBit)

-- | IOCTL for DRM is restarted on interruption
-- Apply this function to your preferred ioctl function
drmIoctl :: IOCTL -> IOCTL
drmIoctl = repeatIoctl

-- | Retrieve Controller (and encoder) controling a connector (if any)
getConnectorController :: IOCTL -> FileDescriptor -> Connector -> SysRet (Maybe Controller, Maybe Encoder)
getConnectorController ioctl fd conn = do
   -- Maybe EncoderID
   let encId = connEncoderID conn

   -- Maybe Encoder
   enc <- traverse (getEncoder ioctl fd) encId

   case enc of
      Nothing        -> return (Right (Nothing,Nothing))
      Just (Left err)-> return (Left err)
      Just (Right e) -> do
         -- Maybe ControllerID
         let crtcId = encoderControllerID e

         -- Maybe Controller
         crtc <- traverse (getController ioctl fd) crtcId
         return $ case crtc of
            Nothing        -> Right (Nothing,Just e)
            Just (Left err)-> Left err
            Just (Right c) -> Right (Just c,Just e)

-- | Select elements in the list if the bit corresponding to their index is set in the mask
fromMaskedList :: Bits a => a -> [b] -> [b]
fromMaskedList mask xs = foldr f [] xs'
   where
      xs' = [0..] `zip` xs
      f (k,v) vs
         | testBit mask k = v:vs
         | otherwise      = vs

-- | Retrieve Controllers that can work with the given encoder
getEncoderControllers :: Card -> Encoder -> [ControllerID]
getEncoderControllers res enc = 
   fromMaskedList (encoderPossibleControllers enc) (cardControllerIDs res)

-- | Retrieve Connectors that can work with the given encoder
getEncoderConnectors :: Card -> Encoder -> [ConnectorID]
getEncoderConnectors res enc = 
   fromMaskedList (encoderPossibleConnectors enc) (cardConnectorIDs res)
