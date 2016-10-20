{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Read/write
module ViperVM.Arch.Linux.FileSystem.ReadWrite
   ( IOVec(..)
   -- * Read
   , sysRead
   , sysReadWithOffset
   , sysReadMany
   , sysReadManyWithOffset
   , handleRead
   , handleReadBuffer
   -- * Write
   , sysWrite
   , sysWriteWithOffset
   , sysWriteMany
   , sysWriteManyWithOffset
   , writeBuffer
   )
where

import Foreign.Marshal.Array (withArray)

import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Word (Word64, Word32)
import ViperVM.Format.Binary.Bits (shiftR)
import ViperVM.Format.Binary.Buffer
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Utils.Flow
import ViperVM.Utils.Types.Generics (Generic)


-- | Entry for vectors of buffers
data IOVec = IOVec
   { iovecPtr  :: Ptr ()
   , iovecSize :: Word64
   } deriving (Generic,CStorable)

instance Storable IOVec where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | Read cound bytes from the given file descriptor and put them in "buf"
-- Returns the number of bytes read or 0 if end of file
sysRead :: Handle -> Ptr () -> Word64 -> IOErr Word64
sysRead (Handle fd) ptr count =
   onSuccess (syscall_read fd ptr count) fromIntegral

-- | Read a file descriptor at a given position
sysReadWithOffset :: Handle -> Word64 -> Ptr () -> Word64 -> IOErr Word64
sysReadWithOffset (Handle fd) offset ptr count =
   onSuccess (syscall_pread64 fd ptr count offset) fromIntegral

-- | Read "count" bytes from a handle (starting at optional "offset") and put
-- them at "ptr" (allocated memory should be large enough).  Returns the number
-- of bytes read or 0 if end of file
handleRead :: Handle -> Maybe Word64 -> Ptr () -> Word64 -> IOErr Word64
handleRead hdl Nothing       = sysRead hdl
handleRead hdl (Just offset) = sysReadWithOffset hdl offset

-- | Read n bytes in a buffer
handleReadBuffer :: Handle -> Maybe Word64 -> Word64 -> IOErr Buffer
handleReadBuffer hdl offset size = do
   b <- mallocBytes (fromIntegral size)
   handleRead hdl offset b (fromIntegral size)
      -- free the pointer on error
      >..~=> const (free b)
      -- otherwise return the buffer
      >.~.> \sz -> bufferUnsafePackPtr (fromIntegral sz) (castPtr b)


-- | Like read but uses several buffers
sysReadMany :: Handle -> [(Ptr a, Word64)] -> IOErr Word64
sysReadMany (Handle fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall_readv fd bufs' count) fromIntegral

-- | Like readMany, with additional offset in file
sysReadManyWithOffset :: Handle -> Word64 -> [(Ptr a, Word64)] -> IOErr Word64
sysReadManyWithOffset (Handle fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall_preadv fd bufs' count ol oh) fromIntegral

-- | Write cound bytes into the given file descriptor from "buf"
-- Returns the number of bytes written (0 indicates that nothing was written)
sysWrite :: Handle -> Ptr a -> Word64 -> IOErr Word64
sysWrite (Handle fd) buf count =
   onSuccess (syscall_write fd buf count) fromIntegral

-- | Write a file descriptor at a given position
sysWriteWithOffset :: Handle -> Word64 -> Ptr () -> Word64 -> IOErr Word64
sysWriteWithOffset (Handle fd) offset buf count =
   onSuccess (syscall_pwrite64 fd buf count offset) fromIntegral


-- | Like write but uses several buffers
sysWriteMany :: Handle -> [(Ptr a, Word64)] -> IOErr Word64
sysWriteMany (Handle fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall_writev fd bufs' count) fromIntegral

-- | Like writeMany, with additional offset in file
sysWriteManyWithOffset :: Handle -> Word64 -> [(Ptr a, Word64)] -> IOErr Word64
sysWriteManyWithOffset (Handle fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall_pwritev fd bufs' count ol oh) fromIntegral

-- | Write a buffer
writeBuffer :: Handle -> Buffer -> IOErr ()
writeBuffer fd bs = bufferUnsafeUsePtr bs go
   where
      go _ 0     = flowRet0 ()
      go ptr len = sysWrite fd ptr (fromIntegral len)
         >.~^> \c -> go (ptr `indexPtr` fromIntegral c)
                        (len - fromIntegral c)
