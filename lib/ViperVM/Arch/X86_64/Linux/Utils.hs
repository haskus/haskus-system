module ViperVM.Arch.X86_64.Linux.Utils (toSet) where

import Data.Bits (Bits, (.|.))

-- | Convert a list of enum to set (i.e. perform a OR)
toSet :: (Enum a, Num b, Bits b) => [a] -> b
toSet xs = foldr (.|.) 0 (fmap (fromIntegral . fromEnum) xs)
