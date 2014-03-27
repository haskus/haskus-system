module ViperVM.Arch.X86_64.Linux.Memory (
   sysBrk, sysBrkGet, sysBrkSet
) where

import Data.Word (Word64)
import Control.Applicative ((<$>))

import ViperVM.Arch.X86_64.Linux.Syscall

-- | Set program break location (i.e. data segement size)
-- 
-- On failure, Linux returns the current value. Failures include a value
-- inferior to the end of the data segment, mmaped regions already existing in the
-- region we allocate, etc.  
-- On success, the returned value is the new program break address (i.e. the given parameter).
sysBrk :: Word64 -> IO Word64
sysBrk addr = fromIntegral <$> syscall1 12 addr

-- | Return current program break
-- We call sysBrk with an invalid value
sysBrkGet :: IO Word64
sysBrkGet = sysBrk 0

-- | Try to set program break and returns True on success
sysBrkSet :: Word64 -> IO Bool
sysBrkSet addr = (==addr) <$> sysBrk addr
