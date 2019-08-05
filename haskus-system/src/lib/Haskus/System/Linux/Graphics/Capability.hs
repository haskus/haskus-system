{-# LANGUAGE DataKinds #-}

-- | Graphic card capabilities
module Haskus.System.Linux.Graphics.Capability
   ( getCapability
   , getCapabilityErr
   , getBoolCapability
   , Capability (..)
   , ClientCapability (..)
   , setClientCapability
   )
where

import Haskus.System.Linux.Graphics.KIO
import Haskus.System.Linux.Handle
import Haskus.System.Linux.ErrorCode
import Haskus.Binary.Enum
import Haskus.Number.Word
import Haskus.Utils.Flow

-- | Get a capability
--
-- Return 0 on error
getCapability :: MonadInIO m => Handle -> Capability -> m Word64
getCapability hdl cap = do
   getCapabilityErr hdl cap
      |> catchEvalE (const (pure 0))

-- | Get a capability or an error code
getCapabilityErr :: MonadInIO m => Handle -> Capability -> Excepts '[ErrorCode] m Word64
getCapabilityErr hdl cap = do
   let s = StructGetCap (toEnumField cap) 0
   ioctlGetCapabilities s hdl ||> gcValue

-- | Indicate if a capability is supported
getBoolCapability :: MonadInIO m => Handle -> Capability -> m Bool
getBoolCapability hdl cap = getCapability hdl cap ||> (/= 0)

-- | Set a client capability
setClientCapability :: MonadInIO m => Handle -> ClientCapability -> Bool -> Excepts '[ErrorCode] m ()
setClientCapability hdl cap b = do
   let 
      v = if b then 1 else 0
      s = StructSetClientCap (toEnumField cap) v
   void (ioctlSetClientCapability s hdl)
