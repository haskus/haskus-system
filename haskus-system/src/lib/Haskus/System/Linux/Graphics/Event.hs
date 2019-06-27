{-# LANGUAGE ScopedTypeVariables #-}

-- | Graphics events
module Haskus.System.Linux.Graphics.Event
   ( Event(..)
   , peekEvents
   , EventType (..)
   , EventHeader (..)
   , VBlankEventData (..)
   , SequenceEventData (..)
   )
where

import Haskus.Format.Binary.Word
import Foreign.Ptr
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Storable
import Haskus.System.Linux.Internals.Graphics
import Haskus.Utils.Monad
import Haskus.Utils.Flow

-- | Graphics events
data Event
   = VBlankEvent   VBlankEventData     -- ^ Beginning of the VBlank perriod
   | FlipCompleteEvent VBlankEventData -- ^ Page flipping complete
   | SequenceEvent SequenceEventData   -- ^ Controller sequence event
   | CustomEvent   EventHeader Buffer  -- ^ Custom event
   deriving (Show)


-- | Peek events
peekEvents :: forall m. MonadIO m => Ptr () -> Word32 -> m [Event]
peekEvents = go
   where
      go _ 0 = return []
      go p r = do
         (ev,len) <- peekEvent p
         evs <- go (p `plusPtr` fromIntegral len) (r - len)
         return (ev:evs)

      peekEvent :: Ptr () -> m (Event,Word32)
      peekEvent ptr = do
         hdr <- peek (castPtr ptr)
         let payloadPtr = castPtr ptr `plusPtr` 8 -- sizeof event header
         v <- case toEventType (eventType hdr) of
            VBlankEventType       -> VBlankEvent       <|| peek payloadPtr
            FlipCompleteEventType -> FlipCompleteEvent <|| peek payloadPtr
            SequenceEventType     -> SequenceEvent     <|| peek payloadPtr
            CustomEventType _     -> CustomEvent hdr <||
               bufferPackPtr (fromIntegral (eventLength hdr) - 8) payloadPtr
                               
         return (v,eventLength hdr)
