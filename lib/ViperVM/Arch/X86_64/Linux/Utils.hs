module ViperVM.Arch.X86_64.Linux.Utils (
   toSet, withMaybeOrNull
) where

import Data.Bits (Bits, (.|.))
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

-- | Convert a list of enum to set (i.e. perform a OR)
toSet :: (Enum a, Num b, Bits b) => [a] -> b
toSet xs = foldr (.|.) 0 (fmap (fromIntegral . fromEnum) xs)


withMaybeOrNull :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybeOrNull s f = case s of
   Nothing -> f nullPtr
   Just x -> with x f
