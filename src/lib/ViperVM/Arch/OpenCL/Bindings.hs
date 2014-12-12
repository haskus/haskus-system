-- | Helpers for OpenCL bindings
module ViperVM.Arch.OpenCL.Bindings where

import ViperVM.Utils.EnumSet as ES
import Data.Bits

-- | Data convertible from and to an OpenCL constant value
class Enum a => CLConstant a where
   toCL :: Integral b => a -> b
   fromCL :: Integral b => b -> a


-- | Data convertible from and to an OpenCL bitset
class Enum a => CLSet a where
   toCLSet :: (Bits b, Integral b) => [a] -> b
   toCLSet = ES.unwrap . ES.fromList

   fromCLSet :: (Bits b, Integral b) => b -> [a]
   fromCLSet = ES.toList . ES.EnumSet
