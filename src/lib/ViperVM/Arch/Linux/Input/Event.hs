{-# LANGUAGE DeriveGeneric #-}

module ViperVM.Arch.Linux.Input.Event
   ( Event (..)
   , EventType(..)
   )
where

import Data.Word
import Data.Int
import GHC.Generics (Generic)

import ViperVM.Arch.X86_64.Linux.Time (TimeVal)

-- | Input event
data Event = Event
   { eventTime  :: TimeVal
   , eventType  :: Word16
   , eventCode  :: Word16
   , eventValue :: Int32
   } deriving (Show,Eq,Generic)

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
