module ViperVM.Platform.Memory.MultiData
   ( MultiData(..)
   )
where

import ViperVM.Platform.Types (Data)
import ViperVM.STM.TList

-- | A data with possibly more than one instance
--
-- * parameterized with p
-- * stored in different locations and with potentially different representation r
data MultiData p r s = MultiData
   { mdParameters :: p              -- ^ Parameters of the data
   , mdInstances  :: TList (r,Data) -- ^ Data instances
   , mdSources    :: TList s        -- ^ Sources (data from which we can obtain this one using s)
   , mdTargets    :: TList s        -- ^ Targets (data that can/must use our data to create theirs)
   }
