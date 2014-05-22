module ViperVM.Platform.Transfer (
   Transfer(..),
   networkTransfer
) where

import Control.Concurrent.STM

import ViperVM.Platform.Topology
import ViperVM.Platform.TransferResult
import ViperVM.Platform.Drivers (transferRegion)

data Transfer = Transfer {
   transferResult :: TMVar TransferResult
}

networkTransfer :: Network -> BufferData -> BufferData -> IO Transfer
networkTransfer net src dst = do
   return undefined
