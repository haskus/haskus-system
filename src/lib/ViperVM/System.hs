-- | System
module ViperVM.System
   ( module X
   , forM_
   , forM
   , traverse_
   , traverse
   , void
   , forever
   )
where

import ViperVM.System.Devices  as X
import ViperVM.System.Event    as X
import ViperVM.System.Graphics as X
import ViperVM.System.Input    as X
import ViperVM.System.Process  as X
import ViperVM.System.Sys      as X
import ViperVM.System.System   as X
import ViperVM.System.Terminal as X
import ViperVM.System.Power    as X


-- useful helpers
import Data.Foldable (forM_,traverse_)
import Data.Traversable (forM)
import Control.Monad (void,forever)
