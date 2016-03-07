-- | Graphic card capabilities
module ViperVM.Arch.Linux.Graphics.Capability
   ( getCapability
   , supports
   , Capability (..)
   , ClientCapability (..)
   , setClientCapability
   )
where

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Arch.Linux.ErrorCode

import Data.Word
import Control.Monad (void)

-- | Get a capability
getCapability :: Card -> Capability -> SysRet Word64
getCapability card cap = do
   let s = StructGetCap (fromIntegral (fromEnum cap + 1)) 0
   fmap gcValue <$> ioctlGetCapabilities (cardHandle card) s

-- | Indicate if a capability is supported
supports :: Card -> Capability -> SysRet Bool
supports card cap = fmap (/= 0) <$> getCapability card cap

-- | Set a client capability
setClientCapability :: Card -> ClientCapability -> Bool -> SysRet ()
setClientCapability card cap b = do
   let 
      v = if b then 1 else 0
      s = StructSetClientCap (fromIntegral (fromEnum cap + 1)) v
   void <$> ioctlSetClientCapability (cardHandle card) s
