{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ViperVM.Utils.Unsafe
   ( inlinePerformIO
   )
where

import GHC.IO (IO(IO))
import GHC.Base (realWorld#)

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
--
-- Taken from Data.Text.Internal.Unsafe
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
