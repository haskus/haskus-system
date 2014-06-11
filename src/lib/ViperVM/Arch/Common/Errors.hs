module ViperVM.Arch.Common.Errors where

-- | Buffer allocation error
data AllocError = 
     AllocOutOfMemory
   | AllocUnknownError
   deriving (Show,Eq)
