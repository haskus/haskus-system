{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE BlockArguments #-}

module Haskus.System.FileSystem
   ( withOpenAt
   , atomicReadBuffer
   , readBuffer
   , readStorable
   , HandleFlag(..)
   , FilePermission(..)
   )
where


import Haskus.System.Linux.Handle
import Haskus.System.Linux.FileSystem
import Haskus.System.Linux.FileSystem.ReadWrite
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.System.Sys
import Haskus.Utils.Flow
import Haskus.Utils.Types.List
import Haskus.Format.Text

-- | Open at
withOpenAt :: forall xs zs m a.
   ( LiftVariant OpenErrors zs
   , LiftVariant xs zs
   , zs ~ Union xs OpenErrors
   , MonadInIO m
   ) => Handle -> FilePath -> HandleFlags -> FilePermissions -> (Handle -> Excepts xs m a) -> Excepts zs m a
withOpenAt fd path flags perm act = do
      fd2 <- liftE (open (Just fd) path flags perm)
      liftE (act fd2) |> finallyE (runE_ (close fd2))

-- | Read a file with a single "read"
--
-- Some files (e.g., in procfs) need to be read atomically to ensure that their
-- contents is valid. In this function, we increase the buffer size until we can
-- read the whole file in it with a single "read" call.
atomicReadBuffer :: Handle -> FilePath -> Excepts (Union ReadErrors' OpenErrors) Sys Buffer
atomicReadBuffer hdl path = withOpenAt hdl path BitSet.empty BitSet.empty (go 2000)
   where
      go :: Word64 -> Handle -> Excepts ReadErrors' Sys Buffer
      go sz fd = do
         -- use 0 offset to read from the beginning
         buf <- handleReadBuffer fd (Just 0) sz
                  |> onE \err -> do
                        let msg = textFormat ("Atomic read file (failed with " % shown % ")") err
                        sysLog LogWarning msg

         if fromIntegral (bufferSize buf) == sz
            then go (sz*2) fd
            else return buf


-- | Read into a buffer
readBuffer :: Handle -> Maybe Word64 -> Word64 -> Excepts ReadErrors' Sys Buffer
readBuffer hdl moffset size = handleReadBuffer hdl moffset size

-- | Read a storable
readStorable :: forall a. Storable a => Handle -> Maybe Word64 -> Excepts ReadErrors' Sys a
readStorable hdl moffset = readBuffer hdl moffset (sizeOfT' @a)
   ||> bufferPeekStorable
