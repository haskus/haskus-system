module ViperVM.Platform.Memory.MultiData
   ( MultiData(..)
   )
where

import ViperVM.Platform.Memory.Data
import ViperVM.STM.TList

data MultiData p i = MultiData
   { mdInstances :: TList (i,Data)
   , mdPrototype :: p
   }
