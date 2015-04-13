module ViperVM.Arch.Linux.Input.Relative
   ( RelativeEvent(..)
   )
where

data RelativeEvent
   = RelativeX
   | RelativeY
   | RelativeRX
   | RelativeRY
   | RelativeHorizontalWheel
   | RelativeDial
   | RelativeWheel
   | RelativeMisc
   deriving (Show,Eq,Enum,Bounded)
