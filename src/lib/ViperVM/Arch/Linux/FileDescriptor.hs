-- | File descriptor (used for many things in Linux)
module ViperVM.Arch.Linux.FileDescriptor
   ( FileDescriptor(..)
   )
where

-- | File descriptor
newtype FileDescriptor = FileDescriptor Word deriving (Show,Eq)
