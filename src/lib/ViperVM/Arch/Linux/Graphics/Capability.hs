-- | Graphic card capabilities
module ViperVM.Arch.Linux.Graphics.Capability
   ( cardCapability
   , cardHasSupportFor
   , Capability (..)
   )
where

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Arch.Linux.ErrorCode

import Data.Word

-- | Indicate if the given capability is supported
cardCapability :: Card -> Capability -> SysRet Word64
cardCapability card cap = do
   let param = StructGetCap (fromIntegral (fromEnum cap + 1)) 0
   ret <- ioctlGetCapabilities (cardHandle card) param
   case ret of
      Left err -> return (Left err)
      Right (StructGetCap _ value) -> return (Right value)

-- | Indicate if a capability is supported
cardHasSupportFor :: Card -> Capability -> SysRet Bool
cardHasSupportFor card cap = fmap (/= 0) <$> cardCapability card cap

