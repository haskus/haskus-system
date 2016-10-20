-- | Graphics events
module ViperVM.Arch.Linux.Graphics.Event
   ( Event(..)
   , peekEvents
   , EventType (..)
   , StructEventVBlank (..)
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Storable
import ViperVM.Arch.Linux.Internals.Graphics

-- | Graphics events
data Event
   = VBlankEvent EventType StructEventVBlank   -- ^ VBlank event
   | CustomEvent Word32 Buffer                 -- ^ Custom event
   deriving (Show)

-- | Peek events
peekEvents :: Ptr () -> Word32 -> IO [Event]
peekEvents = go
   where
      go _ 0 = return []
      go p r = do
         (ev,len) <- peekEvent p
         evs <- go (p `indexPtr` fromIntegral len) (r - len)
         return (ev:evs)

      peekEvent :: Ptr () -> IO (Event,Word32)
      peekEvent ptr = do
         e <- peek (castPtr ptr)
         v <- case toEventType (eventType e) of
            Just t  -> VBlankEvent t <$> peek (castPtr ptr)
            Nothing -> CustomEvent (eventType e) <$>
               bufferPackPtr (fromIntegral (eventLength e) - 8) (castPtr ptr `indexPtr` 8)
                               
         return (v,eventLength e)
