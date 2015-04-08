-- | OpenCL event module
module ViperVM.Arch.OpenCL.Event
   ( Event(..)
   , waitForEvents
   )
where

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library
import ViperVM.Arch.OpenCL.Bindings
import ViperVM.Arch.OpenCL.Error

import Control.Monad (void)
import Foreign.Marshal.Array (withArray)

-- | OpenCL event
data Event = Event Library Event_ deriving (Eq)

instance Entity Event where 
   unwrap (Event _ x) = x
   cllib (Event l _) = l
   retain = retainEvent
   release = releaseEvent

-- | Wait for events
waitForEvents :: [Event] -> IO CLError
waitForEvents [] = return CL_SUCCESS
waitForEvents evs@(e:_) = let lib = cllib e in
   withArray (fmap unwrap evs) $ \events ->
      fromCL <$> rawClWaitForEvents lib (fromIntegral $ length evs) events

-- | Release a event
releaseEvent :: Event -> IO ()
releaseEvent ctx = void (rawClReleaseEvent (cllib ctx) (unwrap ctx))

-- | Retain a event
retainEvent :: Event -> IO ()
retainEvent ctx = void (rawClRetainEvent (cllib ctx) (unwrap ctx))
