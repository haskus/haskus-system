{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module ViperVM.Arch.Linux.FileSystem.ReadWrite
   ( IOVec(..)
   , sysRead
   , sysReadWithOffset
   , sysReadMany
   , sysReadManyWithOffset
   , sysWrite
   , sysWriteWithOffset
   , sysWriteMany
   , sysWriteManyWithOffset
   , readByteString
   , writeByteString
   )
where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Data.Word (Word64, Word32)
import Foreign.CStorable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)

import GHC.Generics (Generic)

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Syscalls


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
   onSuccess (syscall_read fd buf count) fromIntegral

-- | Read a file descriptor at a given position
sysReadWithOffset :: FileDescriptor -> Word64 -> Ptr () -> Word64 -> SysRet Word64
sysReadWithOffset (FileDescriptor fd) offset buf count =
   onSuccess (syscall_pread64 fd buf count offset) fromIntegral

-- | Like read but uses several buffers
sysReadMany :: FileDescriptor -> [(Ptr a, Word64)] -> SysRet Word64
sysReadMany (FileDescriptor fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall_readv fd bufs' count) fromIntegral

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
      onSuccess (syscall_preadv fd bufs' count ol oh) fromIntegral

-- | Write cound bytes into the given file descriptor from "buf"
-- Returns the number of bytes written (0 indicates that nothing was written)
sysWrite :: FileDescriptor -> Ptr a -> Word64 -> SysRet Word64
sysWrite (FileDescriptor fd) buf count =
   onSuccess (syscall_write fd buf count) fromIntegral

-- | Write a file descriptor at a given position
sysWriteWithOffset :: FileDescriptor -> Word64 -> Ptr () -> Word64 -> SysRet Word64
sysWriteWithOffset (FileDescriptor fd) offset buf count =
   onSuccess (syscall_pwrite64 fd buf count offset) fromIntegral


-- | Like write but uses several buffers
sysWriteMany :: FileDescriptor -> [(Ptr a, Word64)] -> SysRet Word64
sysWriteMany (FileDescriptor fd) bufs =
   let
      toVec (p,s) = IOVec (castPtr p) s
      count = length bufs
   in
   withArray (fmap toVec bufs) $ \bufs' ->
      onSuccess (syscall_writev fd bufs' count) fromIntegral

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
      onSuccess (syscall_pwritev fd bufs' count ol oh) fromIntegral

-- | Read n bytes in a bytestring
readByteString :: FileDescriptor -> Int -> SysRet ByteString
readByteString fd size = do
   b <- mallocBytes size
   ret <- sysRead fd b (fromIntegral size)
   case ret of
      Left err -> return (Left err)
      Right sz -> Right <$> unsafePackCStringLen (b',sz')
         where
            sz' = fromIntegral sz
            b'  = castPtr b

-- | Write a bytestring
writeByteString :: FileDescriptor -> ByteString -> SysRet Word64
writeByteString fd bs = unsafeUseAsCStringLen bs (go 0)
   where
      go n (ptr,0)   = return (Right n)
      go n (ptr,len) = do
         c <- sysWrite fd ptr (fromIntegral len)
         case c of
            Left err -> return (Left err)
            Right c' -> go (n+c') (ptr `plusPtr` fromIntegral c', len - fromIntegral c')
