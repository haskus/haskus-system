{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ViperVM.Arch.Linux.Input.Event
   ( Event (..)
   , EventType(..)
   )
where

import Data.Word
import Data.Int
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr,castPtr)
import Foreign.Storable
import Foreign.CStorable

import ViperVM.Arch.Linux.Time (TimeVal)

-- | Input event
data Event = Event
   { eventTime  :: TimeVal
   , eventType  :: EventType
   , eventCode  :: Word16
   , eventValue :: Int32
   } deriving (Show,Eq,Generic,CStorable)

instance Storable Event where
   alignment = cAlignment
   sizeOf    = cSizeOf
   peek      = cPeek
   poke      = cPoke


-- | Event types
data EventType
   = EventTypeSync
   | EventTypeKey
   | EventTypeRelative
   | EventTypeAbsolute
   | EventTypeMisc
   | EventTypeSwitch
   | EventTypeLED
   | EventTypeSound
   | EventTypeReplay
   | EventTypeForceFeedback
   | EventTypePower
   | EventTypeForceFeedbackStatus
   deriving (Show,Eq)

-- | Event type is represented as a Word16 in Event
instance Storable EventType where
   alignment _ = 2
   sizeOf   _  = 2
   peek ptr    = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr Word16)
   poke ptr v  = poke (castPtr ptr :: Ptr Word16) (fromIntegral (fromEnum v))

instance CStorable EventType where
   cAlignment = alignment
   cSizeOf    = sizeOf
   cPeek      = peek
   cPoke      = poke

instance Enum EventType where
   fromEnum x = case x of
      EventTypeSync                 -> 0x00
      EventTypeKey                  -> 0x01
      EventTypeRelative             -> 0x02
      EventTypeAbsolute             -> 0x03
      EventTypeMisc                 -> 0x04
      EventTypeSwitch               -> 0x05
      EventTypeLED                  -> 0x11
      EventTypeSound                -> 0x12
      EventTypeReplay               -> 0x14
      EventTypeForceFeedback        -> 0x15
      EventTypePower                -> 0x16
      EventTypeForceFeedbackStatus  -> 0x17
   toEnum x = case x of
      0x00 -> EventTypeSync
      0x01 -> EventTypeKey
      0x02 -> EventTypeRelative
      0x03 -> EventTypeAbsolute
      0x04 -> EventTypeMisc
      0x05 -> EventTypeSwitch
      0x11 -> EventTypeLED
      0x12 -> EventTypeSound
      0x14 -> EventTypeReplay
      0x15 -> EventTypeForceFeedback
      0x16 -> EventTypePower
      0x17 -> EventTypeForceFeedbackStatus
      _    -> error "Unknown event type"

