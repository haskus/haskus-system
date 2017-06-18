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
getCapability :: MonadIO m => Handle -> Capability -> Flow m '[Word64,ErrorCode]
getCapability hdl cap = do
   let s = StructGetCap (toEnumField cap) 0
   liftIO (ioctlGetCapabilities s hdl)
      >.-.> gcValue

-- | Indicate if a capability is supported
supports :: MonadIO m => Handle -> Capability -> Flow m '[Bool,ErrorCode]
supports hdl cap = getCapability hdl cap
   >.-.> (/= 0)

-- | Set a client capability
setClientCapability :: MonadIO m => Handle -> ClientCapability -> Bool -> Flow m '[(),ErrorCode]
setClientCapability hdl cap b = do
   let 
      v = if b then 1 else 0
      s = StructSetClientCap (toEnumField cap) v
   liftIO (ioctlSetClientCapability s hdl)
      >.-.> const ()
