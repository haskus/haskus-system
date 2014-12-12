-- | Helpers for OpenCL bindings
module ViperVM.Arch.OpenCL.Bindings where

-- | Data convertible from and to an OpenCL constant value
class Enum a => CLConstant a where
   toCL :: Integral b => a -> b
   fromCL :: Integral b => b -> a
