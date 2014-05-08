module ViperVM.Platform.ProcPeer (
   ProcPeer(..)
) where

import Data.Word (Word)

import qualified ViperVM.Arch.OpenCL.All as CL

-- | Backend specific processor fields
data ProcPeer =
     CPUProc {
         cpuIndex :: Word
     }
   | OpenCLProc {
         clProcDevice :: CL.Device,
         clProcContext :: CL.Context
     }
