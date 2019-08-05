{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Linux input
module Haskus.System.Linux.Input
   ( getSupportedEvents
   )
where

import Haskus.System.Linux.Internals.Input
import Haskus.System.Linux.Handle
import Haskus.System.Linux.ErrorCode
import Haskus.Binary.Buffer
import Haskus.Utils.Flow


-- | Call getDeviceBits until the buffer is large enough to contain all the
-- event codes. Initial buffer size should be sensible size in *bits*.
getDeviceBits :: MonadInIO m => Handle -> Maybe EventType -> Word -> Excepts '[ErrorCode] m Buffer
getDeviceBits hdl ev bitSize = go ((bitSize + 7) `div` 8)
   where
      go sz = do
         (rdsz,b) <- ioctlGetDeviceBits ev (fromIntegral sz) hdl
         -- check that the buffer was large enough and splice it, otherwise retry
         -- with a larger buffer
         if rdsz == fromIntegral sz
            then go (2*sz)
            else return (bufferTake (fromIntegral rdsz) b)


-- | Return the event types supported by the input device
getSupportedEvents :: MonadInIO m => Handle -> Excepts '[ErrorCode] m Buffer
getSupportedEvents hdl = do
   getDeviceBits hdl Nothing 0x20
