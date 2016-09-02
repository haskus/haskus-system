{-# LANGUAGE ScopedTypeVariables #-}

-- | Event management
module ViperVM.System.Event
   ( newEventReader
   , onEvent
   )
where

import Control.Concurrent.STM
import Control.Concurrent
import Data.Foldable (traverse_)
import Prelude hiding (init,tail)
import Control.Monad (forever)
import Foreign.Storable
import Foreign.Marshal (allocaArray, peekArray)
import System.Posix.Types (Fd(..))

import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Utils.Flow
import ViperVM.System.Sys
import ViperVM.System.Process
import ViperVM.Format.Binary.Ptr

-- | Create a new thread reading events and putting them in a TChan
newEventReader :: forall a. Storable a => Handle -> Sys (TChan a)
newEventReader fd@(Handle lowfd) = do
   let
      sz  = sizeOf (undefined :: a)
      rfd = Fd (fromIntegral lowfd)
      nb  = 50 -- number of events read at once

   ch <- sysIO newBroadcastTChanIO
   sysFork $ sysIO $ allocaArray nb $ \ptr -> forever $ do
      threadWaitRead rfd
      sysRead fd (castPtr ptr) (fromIntegral sz * fromIntegral nb)
      >.~!> \sz2 -> do
         -- FIXME: we should somehow signal that an error occured
         evs <- peekArray (fromIntegral sz2 `div` sz) ptr
         atomically $ traverse_ (writeTChan ch) evs
   return ch

-- | Read events in the given channel forever
onEvent :: TChan e -> (e -> Sys ()) -> Sys ()
onEvent bch f = do
   sysLog LogInfo "Creating event listener"

   ch <- sysIO $ atomically $ dupTChan bch
   sysFork $ sysIO $ forever (atomically (readTChan ch) >>= runSys' . f)
