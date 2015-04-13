module ViperVM.Arch.Linux.Input.Misc
   ( MiscEvent(..)
   )
where

data MiscEvent
   = MiscSerial
   | MiscPulseLED
   | MiscGesture
   | MiscRaw
   | MiscScan
   | MiscTimeStamp
   deriving (Eq,Show,Enum)
