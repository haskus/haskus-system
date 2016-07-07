-- | Graphic card capabilities
module ViperVM.Arch.Linux.Graphics.Capability
   ( getCapability
   , supports
   , Capability (..)
   , ClientCapability (..)
   , setClientCapability
   )
where

import ViperVM.Arch.Linux.Internals.Graphics
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Word
import ViperVM.System.Sys
import ViperVM.Utils.Flow

import Control.Monad (void)

-- | Get a capability
getCapability :: Handle -> Capability -> SysRet Word64
getCapability hdl cap = do
   let s = StructGetCap (toEnumField cap) 0
   ioctlGetCapabilities s hdl
      >.-.> gcValue

-- | Indicate if a capability is supported
supports :: Handle -> Capability -> SysRet Bool
supports hdl cap = getCapability hdl cap
   >.-.> (/= 0)

-- | Set a client capability
setClientCapability :: Handle -> ClientCapability -> Bool -> Sys ()
setClientCapability hdl cap b = do
   let 
      v = if b then 1 else 0
      s = StructSetClientCap (toEnumField cap) v
      m = "Set client capability " ++ show cap
   void $ sysCallWarn m (ioctlSetClientCapability s hdl)
