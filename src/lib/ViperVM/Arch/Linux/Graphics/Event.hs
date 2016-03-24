module ViperVM.Arch.Linux.Graphics.Event
   ( Event(..)
   , UnknownEventInfo(..)
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

data Event
   = VBlankEvent  EventType StructEventVBlank
   | UnknownEvent Word32 ByteString
   deriving (Show)

data UnknownEventInfo = UnknownEventInfo
   { unknownEventType :: Word32
   , unknownEventData :: ByteString
   }
   deriving (Show)

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
            Nothing -> UnknownEvent (eventType e) <$>
               packCStringLen (castPtr ptr `plusPtr` 8,
                               fromIntegral (eventLength e) - 8)
         return (v,eventLength e)
