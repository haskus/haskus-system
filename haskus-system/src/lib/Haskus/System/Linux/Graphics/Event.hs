{-# LANGUAGE ScopedTypeVariables #-}

-- | Graphics events
module Haskus.System.Linux.Graphics.Event
   ( Event(..)
   , peekEvents
   , EventType (..)
   , DRMEvent (..)
   )
where

import Haskus.Format.Binary.Word
import Foreign.Ptr
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Storable
import Haskus.System.Linux.Internals.Graphics
import Haskus.Utils.Monad

-- | Graphics events
data Event
   = Event EventType DRMEvent  -- ^ Builtin event
   | CustomEvent Word32 Buffer -- ^ Custom event
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
         e <- peek (castPtr ptr)
         v <- case toEventType (eventType e) of
            Just t  -> Event t <$> peek (castPtr ptr)
            Nothing -> CustomEvent (eventType e) <$>
               bufferPackPtr (fromIntegral (eventLength e) - 8) (castPtr ptr `plusPtr` 8)
                               
         return (v,eventLength e)
