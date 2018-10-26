{-# OPTIONS_GHC -freduction-depth=0 #-}

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
withOpenAt :: forall xs zs m.
   ( Liftable OpenErrors zs
   , Liftable xs zs
   , zs ~ Union xs OpenErrors
   , MonadInIO m
   ) => Handle -> FilePath -> HandleFlags -> FilePermissions -> (Handle -> Flow m xs) -> Flow m zs
withOpenAt fd path flags perm act =
   open (Just fd) path flags perm
      >.~|> \fd1 -> do
         res <- act fd1
         void (close fd1)
         return res

-- | Read a file with a single "read"
--
-- Some files (e.g., in procfs) need to be read atomically to ensure that their
-- contents is valid. In this function, we increase the buffer size until we can
-- read the whole file in it with a single "read" call.
atomicReadBuffer :: Handle -> FilePath -> Flow Sys (Buffer ': Union ReadErrors' OpenErrors)
atomicReadBuffer hdl path = withOpenAt hdl path BitSet.empty BitSet.empty (go 2000)
   where
      go :: Word64 -> Handle -> Flow Sys (Buffer ': ReadErrors')
      go sz fd =
         -- use 0 offset to read from the beginning
         handleReadBuffer fd (Just 0) sz
            >..~=> (\err -> do
               let msg = textFormat ("Atomic read file (failed with " % shown % ")") err
               sysLog LogWarning msg)
            >.~^> (\buf ->
               if fromIntegral (bufferSize buf) == sz
                  then go (sz*2) fd
                  else flowSet buf)


-- | Read into a buffer
readBuffer :: Handle -> Maybe Word64 -> Word64 -> Flow Sys (Buffer ': ReadErrors')
readBuffer hdl moffset size = handleReadBuffer hdl moffset size

-- | Read a storable
readStorable :: forall a. Storable a => Handle -> Maybe Word64 -> Flow Sys (a ': ReadErrors')
readStorable hdl moffset = readBuffer hdl moffset (sizeOfT' @a)
   >.-.> bufferPeekStorable
