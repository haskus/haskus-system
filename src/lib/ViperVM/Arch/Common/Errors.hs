module ViperVM.Arch.Common.Errors
   ( AllocError(..)
   )
where

-- | Buffer allocation error
data AllocError
   = AllocOutOfMemory
   | AllocUnknownError
   deriving (Show,Eq)
