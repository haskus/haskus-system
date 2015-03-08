-- | Video controller management
--
-- Controllers are called CRTC in original terminology
module ViperVM.Arch.Linux.Graphics.Controller
   ( Controller(..)
   , getController
   , setController
   )
where

import ViperVM.Arch.Linux.Graphics.LowLevel.Controller


