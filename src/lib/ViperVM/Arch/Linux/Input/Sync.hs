module ViperVM.Arch.Linux.Input.Sync
   ( SyncEvent(..)
   )
where

data SyncEvent
   = SyncReport
   | SyncConfig
   | SyncMultiTouchReport
   | SyncDropped
   deriving (Show,Eq,Enum)
