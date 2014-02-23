module ViperVM.Platform.OpenCL.Event (
   Event(..),
   waitForEvents,
) where

import ViperVM.Platform.OpenCL.Types
import ViperVM.Platform.OpenCL.Entity
import ViperVM.Platform.OpenCL.Library
import ViperVM.Platform.OpenCL.Bindings
import ViperVM.Platform.OpenCL.Error

import Control.Applicative ((<$>))
import Foreign.Marshal.Array (withArray)

data Event = Event Library Event_ deriving (Eq)

instance Entity Event where 
   unwrap (Event _ x) = x
   cllib (Event l _) = l

-- | Wait for events
waitForEvents :: [Event] -> IO CLError
waitForEvents [] = return CL_SUCCESS
waitForEvents evs@(e:_) = let lib = cllib e in
   withArray (fmap unwrap evs) $ \events ->
      fromCL <$> rawClWaitForEvents lib (fromIntegral $ length evs) events
