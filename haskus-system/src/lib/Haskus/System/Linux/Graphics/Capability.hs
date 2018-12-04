{-# LANGUAGE DataKinds #-}

-- | Graphic card capabilities
module Haskus.System.Linux.Graphics.Capability
   ( getCapability
   , supports
   , Capability (..)
   , ClientCapability (..)
   , setClientCapability
   )
where

import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.Handle
import Haskus.System.Linux.ErrorCode
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.Word
import Haskus.Utils.Flow

-- | Get a capability
getCapability :: MonadInIO m => Handle -> Capability -> FlowT '[ErrorCode] m Word64
getCapability hdl cap = do
   let s = StructGetCap (toEnumField cap) 0
   ioctlGetCapabilities s hdl ||> gcValue

-- | Indicate if a capability is supported
supports :: MonadInIO m => Handle -> Capability -> FlowT '[ErrorCode] m Bool
supports hdl cap = getCapability hdl cap ||> (/= 0)

-- | Set a client capability
setClientCapability :: MonadInIO m => Handle -> ClientCapability -> Bool -> FlowT '[ErrorCode] m ()
setClientCapability hdl cap b = do
   let 
      v = if b then 1 else 0
      s = StructSetClientCap (toEnumField cap) v
   void (ioctlSetClientCapability s hdl)
