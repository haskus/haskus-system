-- | File descriptor (used for many things in Linux)
module ViperVM.Arch.Linux.FileDescriptor
   ( FileDescriptor(..)
   )
where

import Data.Word

-- | File descriptor
newtype FileDescriptor = FileDescriptor Word deriving (Show,Eq)


