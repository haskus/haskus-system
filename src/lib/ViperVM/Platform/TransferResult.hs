-- | Data transfer result
module ViperVM.Platform.TransferResult
   ( TransferResult(..)
   , TransferError(..)
   )
where

-- | Result of a transfer
data TransferResult
   = TransferError TransferError
   | TransferSuccess
   deriving (Show,Eq)


-- | Region transfer error
data TransferError
   = ErrTransferIncompatibleRegions
   | ErrTransferInvalid
   | ErrTransferUnknown
   deriving (Show,Eq)

