{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module ViperVM.Arch.X86_64.Linux.FileSystem.ReadWrite
   ( IOVec(..)
   , sysRead
   , sysReadWithOffset
   , sysReadMany
   , sysReadManyWithOffset
   , sysWrite
   , sysWriteWithOffset
   , sysWriteMany
   , sysWriteManyWithOffset
   )
where

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)
import Foreign.CStorable
import Data.Word (Word64, Word32)
import Data.Bits (shiftR)

import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.X86_64.Linux.Syscall


-- | Entry for vectors of buffers
data IOVec = IOVec
   { iovecPtr  :: Ptr ()
   , iovecSize :: Word64
   } deriving (Generic)

instance CStorable IOVec
instance Storable IOVec where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | Read cound bytes from the given file descriptor and put them in "buf"
-- Returns the number of bytes read or 0 if end of file
sysRead :: FileDescriptor -> Ptr a -> Word64 -> SysRet Word64
sysRead (FileDescriptor fd) buf count =
   onSuccess (syscall3 0 fd buf count) fromIntegral

-- | Read a file descriptor at a given position
sysReadWithOffset :: FileDescriptor -> Word64 -> Ptr () -> Word64 -> SysRet Word64
sysReadWithOffset (FileDescriptor fd) offset buf count =
   onSuccess (syscall4 17 fd buf count offset) fromIntegral

-- | Like read but uses several buffers
sysReadMany :: FileDescriptor -> [(Ptr a, Word64)] -> SysRet Word64
sysReadMany (FileDescriptor fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall3 19 fd bufs' count) fromIntegral

-- | Like readMany, with additional offset in file
sysReadManyWithOffset :: FileDescriptor -> Word64 -> [(Ptr a, Word64)] -> SysRet Word64
sysReadManyWithOffset (FileDescriptor fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall5 295 fd bufs' count ol oh) fromIntegral

-- | Write cound bytes into the given file descriptor from "buf"
-- Returns the number of bytes written (0 indicates that nothing was written)
sysWrite :: FileDescriptor -> Ptr a -> Word64 -> SysRet Word64
sysWrite (FileDescriptor fd) buf count =
   onSuccess (syscall3 1 fd buf count) fromIntegral

-- | Write a file descriptor at a given position
sysWriteWithOffset :: FileDescriptor -> Word64 -> Ptr () -> Word64 -> SysRet Word64
sysWriteWithOffset (FileDescriptor fd) offset buf count =
   onSuccess (syscall4 18 fd buf count offset) fromIntegral


-- | Like write but uses several buffers
sysWriteMany :: FileDescriptor -> [(Ptr a, Word64)] -> SysRet Word64
sysWriteMany (FileDescriptor fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall3 20 fd bufs' count) fromIntegral

-- | Like writeMany, with additional offset in file
sysWriteManyWithOffset :: FileDescriptor -> Word64 -> [(Ptr a, Word64)] -> SysRet Word64
sysWriteManyWithOffset (FileDescriptor fd) offset bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
      -- offset is split in 32-bit words
      ol = fromIntegral offset :: Word32
      oh = fromIntegral (offset `shiftR` 32) :: Word32
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall5 296 fd bufs' count ol oh) fromIntegral
