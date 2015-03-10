-- | Graphic card connector management
module ViperVM.Arch.Linux.Graphics.Connector
   ( Connector(..)
   , Connection(..)
   , SubConnectorType(..)
   , ConnectorType(..)
   , SubPixel(..)
   , cardConnectorController
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.Connector
import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.LowLevel.IDs
import ViperVM.Arch.Linux.Graphics.Card

import Data.Traversable (traverse)

-- | Retrieve Controller (and encoder) controling a connector (if any)
cardConnectorController :: IOCTL -> Card -> Connector -> SysRet (Maybe Controller, Maybe Encoder)
cardConnectorController ioctl card conn = do
   let fd = cardFileDescriptor card
   -- Maybe EncoderID
   let encId = connEncoderID conn

   -- Maybe Encoder
   enc <- traverse (cardEncoderFromID ioctl card) encId

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

