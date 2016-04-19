-- | Graphics events
module ViperVM.Arch.Linux.Graphics.Event
   ( Event(..)
   , peekEvents
   , EventType (..)
   , StructEventVBlank (..)
   )
where

import Data.ByteString
import Foreign.Storable
import Foreign.Ptr
import Data.Word

import ViperVM.Arch.Linux.Internals.Graphics

-- | Graphics events
data Event
   = VBlankEvent EventType StructEventVBlank   -- ^ VBlank event
   | CustomEvent Word32 ByteString             -- ^ Custom event
   deriving (Show)

-- | Peek events
peekEvents :: Ptr () -> Word32 -> IO [Event]
peekEvents = go
   where
      go _ 0 = return []
      go p r = do
         (ev,len) <- peekEvent p
         evs <- go (p `plusPtr` fromIntegral len) (r - len)
         return (ev:evs)

      peekEvent :: Ptr () -> IO (Event,Word32)
      peekEvent ptr = do
         e <- peek (castPtr ptr)
         v <- case toEventType (eventType e) of
            Just t  -> VBlankEvent t <$> peek (castPtr ptr)
            Nothing -> CustomEvent (eventType e) <$>
               packCStringLen (castPtr ptr `plusPtr` 8,
                               fromIntegral (eventLength e) - 8)
         return (v,eventLength e)
