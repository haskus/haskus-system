module ViperVM.Platform.TransferResult where

data TransferResult = 
     TransferError TransferError
   | TransferSuccess


-- | Region transfer error
data TransferError =
     ErrTransferIncompatibleRegions
   | ErrTransferInvalid
   | ErrTransferUnknown
   deriving (Show,Eq)

