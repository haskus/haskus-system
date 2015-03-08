{-# LANGUAGE ScopedTypeVariables #-}

-- | Graphic card connector management
module ViperVM.Arch.Linux.Graphics.Connector
   ( Connector(..)
   , Connection(..)
   , SubConnectorType(..)
   , ConnectorType(..)
   , SubPixel(..)
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.Connector
