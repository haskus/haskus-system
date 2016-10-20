{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Linux input
module ViperVM.Arch.Linux.Input
   ( getSupportedEvents
   )
where

import ViperVM.Arch.Linux.Internals.Input
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Format.Binary.Buffer
import ViperVM.Utils.Flow


-- | Call getDeviceBits until the buffer is large enough to contain all the
-- event codes. Initial buffer size should be sensible size in *bits*.
getDeviceBits' :: Handle -> Maybe EventType -> Word -> IOErr Buffer
getDeviceBits' hdl ev bitSize = go ((bitSize + 7) `div` 8)
   where
      go sz = do
         getDeviceBits ev (fromIntegral sz) hdl
            -- check that the buffer was large enough and splice it, otherwise retry
            -- with a larger buffer
            >.~$> (\(rdsz,b) -> if rdsz == fromIntegral sz
                        then go (2*sz)
                        else flowSet (bufferTake (fromIntegral rdsz) b)
                  )


-- | Return the event types supported by the input device
getSupportedEvents :: Handle -> IOErr Buffer
getSupportedEvents hdl = do
   getDeviceBits' hdl Nothing 0x20
