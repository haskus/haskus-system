{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Event management
module Haskus.System.Event
   ( newEventReader
   , onEvent
   , onEventWithData
   )
where

import Prelude hiding (init,tail)

import Haskus.System.Linux.Handle
import Haskus.System.Linux.FileSystem.ReadWrite
import Haskus.Utils.Flow
import Haskus.Utils.STM
import Haskus.System.Sys
import Haskus.System.Process
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Storable

-- | Create a new thread reading events and putting them in a TChan
newEventReader :: forall a. Storable a => Handle -> Sys (TChan a)
newEventReader h = do
   let
      sz  = sizeOfT @a
      nb  = 50 -- number of events read at once

   ch <- newBroadcastTChanIO
   sysFork "Event reader" $ allocaArray nb $ \ptr -> forever $ do
      threadWaitRead h
      sysRead h (castPtr ptr) (fromIntegral sz * fromIntegral nb)
         >.~!> \sz2 -> do
            -- FIXME: we should somehow signal that an error occured
            evs <- peekArray (fromIntegral sz2 `div` fromIntegral sz) ptr
            atomically (mapM_ (writeTChan ch) evs)
   return ch

-- | Read events in the given channel forever
onEvent :: TChan e -> (e -> Sys ()) -> Sys ()
onEvent bch f = onEventWithData () bch (const f)

-- | Read events in the given channel forever, pass a user-defined data
onEventWithData :: a -> TChan e -> (a -> e -> Sys a) -> Sys ()
onEventWithData x bch f = do
   sysLog LogInfo "Creating event listener"

   ch <- atomically $ dupTChan bch
   sysFork "TChan event listener" $ do
      let
         go a = do
            e  <- atomically (readTChan ch)
            a' <- f a e
            go a'
      go x
