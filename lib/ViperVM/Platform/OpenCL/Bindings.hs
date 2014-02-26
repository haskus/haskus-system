module ViperVM.Platform.OpenCL.Bindings where

import Data.Bits
import Data.Maybe

class Enum a => CLConstant a where
   toCL :: Integral b => a -> b
   fromCL :: Integral b => b -> a

class Enum a => CLSet a where
   toCLSet :: (Bits b, Integral b) => [a] -> b
   toCLSet = sum . map f
      where f = shiftL 1 . fromIntegral . fromEnum

   fromCLSet :: (Bits b, Integral b) => b -> [a]
   fromCLSet x = mapMaybe f (enumFrom (toEnum 0))
      where f e = if testBit x (fromEnum e)
                        then Just e
                        else Nothing

