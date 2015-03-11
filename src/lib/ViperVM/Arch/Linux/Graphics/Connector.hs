{-# LANGUAGE LambdaCase #-}

-- | Graphic card connector management
module ViperVM.Arch.Linux.Graphics.Connector
   ( Connector(..)
   , Connection(..)
   , SubConnectorType(..)
   , ConnectorType(..)
   , SubPixel(..)
   , connectorEncoder
   , connectorController
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.Encoder
import ViperVM.Arch.Linux.Graphics.LowLevel.Connector
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Controller

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either

-- | Encoder attached to the connector, if any
connectorEncoder :: Connector -> SysRet (Maybe Encoder)
connectorEncoder conn = case connectorEncoderID conn of
   Nothing    -> return (Right Nothing)
   Just encId -> fmap Just <$> cardEncoderFromID (connectorCard conn) encId

-- | Retrieve Controller (and encoder) controling a connector (if any)
connectorController :: Connector -> SysRet (Maybe Controller, Maybe Encoder)
connectorController conn = runEitherT $ 
   EitherT (connectorEncoder conn) >>= \case
      Nothing  -> right (Nothing,Nothing)
      Just enc -> EitherT (encoderController enc) >>= \case
         Nothing   -> right (Nothing,Just enc)
         Just cont -> right (Just cont,Just enc)

