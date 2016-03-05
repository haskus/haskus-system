{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ViperVM.Arch.Linux.Graphics.Event
   ( EventType(..)
   , toEventType
   , Event(..)
   , VBlankEventInfo(..)
   , UnknownEventInfo(..)
   , peekEvents
   )
where

import Data.ByteString
import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr
import Data.Word
import GHC.Generics (Generic)

data EventType
   = VBlank
   | FlipComplete
   deriving (Show)

-- | Try to recognize the event type
toEventType :: Word32 -> Maybe EventType
toEventType v = case v of
   0x01 -> Just VBlank
   0x02 -> Just FlipComplete
   _    -> Nothing

data Event
   = VBlankEvent VBlankEventInfo
   | UnknownEvent Word32 ByteString
   deriving (Show)

-- | DRM event info
--
-- Correspond to struct drm_event_vblank
data VBlankEventInfo = VBlankEventInfo
   { vblankEventType         :: Word32
   , vblankEventSize         :: Word32
   , vblankEventUserData     :: Word64
   , vblankEventSeconds      :: Word32
   , vblankEventMicroseconds :: Word32
   , vblankEventSequence     :: Word32
   --, eventReserved   :: Word32
   } 
   deriving (Show,Generic,CStorable)

instance Storable  VBlankEventInfo where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

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
         typ <- peekByteOff (castPtr ptr :: Ptr Word32) 0
         len <- peekByteOff (castPtr ptr :: Ptr Word32) 4
         case toEventType typ of
            Just VBlank       -> do
               ev <- peek (castPtr ptr :: Ptr VBlankEventInfo)
               return (VBlankEvent ev, len)
            Just FlipComplete -> do
               ev <- peek (castPtr ptr :: Ptr VBlankEventInfo)
               return (VBlankEvent ev, len)
            Nothing           -> do
               bs  <- packCStringLen ((castPtr ptr :: Ptr Word8) `plusPtr` 8, fromIntegral len)
               return (UnknownEvent typ bs, len)
