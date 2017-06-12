{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pipe
module Haskus.Arch.Linux.Pipe
   ( createPipe
   )
where

import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Syscalls
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Storable
import Haskus.Utils.Flow

-- | Create a pipe
createPipe :: MonadInIO m => Flow m '[(Handle, Handle),ErrorCode]
createPipe =
   allocaArray 2 $ \(ptr :: Ptr Word) ->
      liftIO (syscall_pipe (castPtr ptr))
         ||> toErrorCode
         >.~.> (const ((,)
            <$> (Handle <$> peekElemOff ptr 0)
            <*> (Handle <$> peekElemOff ptr 1)))
      
