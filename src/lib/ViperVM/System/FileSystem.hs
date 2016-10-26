{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViperVM.System.FileSystem
   ( withOpenAt
   , atomicReadBuffer
   , readBuffer
   , readStorable
   , HandleFlag(..)
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
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.System.Sys
import ViperVM.Utils.Flow
import ViperVM.Utils.Types
import ViperVM.Utils.Types.List

-- | Open at
withOpenAt :: 
   ( Liftable '[ErrorCode] zs
   , Liftable xs zs
   , zs ~ Union xs '[ErrorCode]
   ) => Handle -> FilePath -> HandleFlags -> FilePermissions -> (Handle -> Flow Sys xs) -> SysV zs
withOpenAt fd path flags perm act =
   sysIO (sysOpenAt fd path flags perm)
      >.~|> \fd1 -> do
         res <- act fd1
         sysCallAssertQuiet "Close file" $ sysClose fd1
         return res

-- | Read a file with a single "read"
--
-- Some files (e.g., in procfs) need to be read atomically to ensure that their
-- contents is valid. In this function, we increase the buffer size until we can
-- read the whole file in it with a single "read" call.
atomicReadBuffer :: Handle -> FilePath -> SysV '[Buffer,ErrorCode]
atomicReadBuffer hdl path = withOpenAt hdl path BitSet.empty BitSet.empty (go 2000)
   where
      go :: Word64 -> Handle -> SysV '[Buffer,ErrorCode]
      go sz fd =
         sysCallWarnQuiet "atomic read file"
            -- use 0 offset to read from the beginning
            (handleReadBuffer fd (Just 0) sz)
         >.~^> \buf ->
            if fromIntegral (bufferSize buf) == sz
               then go (sz*2) fd
               else flowSet buf


-- | Read into a buffer
readBuffer :: Handle -> Maybe Word64 -> Word64 -> SysV '[Buffer,ErrorCode]
readBuffer hdl moffset size = sysIO (handleReadBuffer hdl moffset size)

-- | Read a storable
readStorable :: forall a. Storable a => Proxy a -> Handle -> Maybe Word64 -> SysV '[a,ErrorCode]
readStorable _ hdl moffset = readBuffer hdl moffset size >.-.> bufferPeekStorable
   where
      size = sizeOfT' @a
