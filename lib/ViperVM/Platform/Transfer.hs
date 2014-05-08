module ViperVM.Platform.Transfer (
   TransferError(..)
) where

-- | Region transfer error
data TransferError =
     ErrTransferIncompatibleRegions
   | ErrTransferInvalid
   | ErrTransferUnknown
   deriving (Show,Eq)
