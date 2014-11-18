module ViperVM.Platform.Processing.Kernel
   ( Kernel(..)
   , Task(..)
   , TaskParameter(..)
   )
where

import ViperVM.Platform.Drivers (KernelPeer)

import ViperVM.Platform.Types (Buffer, Data, MultiData)

data Kernel = Kernel
   { kernelPeer :: KernelPeer
   }


data Task = Task
   { taskKernel :: Kernel              -- ^ Kernel
   , taskParameters :: [TaskParameter] -- ^ Parameters
   }


data TaskParameter
   = ParamBuffer Buffer
   | ParamData   Data
   | ParamMultiData MultiData
