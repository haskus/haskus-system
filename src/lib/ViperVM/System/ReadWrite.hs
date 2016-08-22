{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ViperVM.System.ReadWrite
   ( withOpenAt
   , handleAtomicReadBufferAt
   , HandleFlag(..)
   , Device(..)
   , FilePermission(..)
   )
where


import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Utils.HList

-- | Open at
withOpenAt :: 
   ( Liftable '[ErrorCode] zs
   , Liftable xs zs
   , zs ~ Fusion xs '[ErrorCode]
   ) => Handle -> FilePath -> HandleFlags -> FilePermissions -> (Handle -> Flow Sys xs) -> SysV zs
withOpenAt fd path flags perm act =
   sysCallWarn ("Open file \""++ path ++ "\"") (sysOpenAt fd path flags perm)
      >.~&> \fd1 -> do
         res <- act fd1
         sysCallAssert "Close file" $ sysClose fd1
         return res

-- | Read a file with a single "read"
--
-- Some files (e.g., in procfs) need to be read atomically to ensure that their
-- contents is valid. In this function, we increase the buffer size until we can
-- read the whole file in it with a single "read" call.
handleAtomicReadBufferAt :: Handle -> FilePath -> SysV '[Buffer,ErrorCode]
handleAtomicReadBufferAt hdl path = withOpenAt hdl path BitSet.empty BitSet.empty (go 2000)
   where
      go :: Word64 -> Handle -> SysV '[Buffer,ErrorCode]
      go sz fd =
         sysCallWarn "atomic read file"
            -- use 0 offset to read from the beginning
            (handleReadBuffer fd (Just 0) sz)
         >.~#> \buf ->
            if fromIntegral (bufferSize buf) == sz
               then go (sz*2) fd
               else flowSet buf


