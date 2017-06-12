{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | System info (uname)
module Haskus.Arch.Linux.Info
   ( SystemInfo(..)
   , systemInfo
   )
where

import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Syscalls
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Ptr
import Haskus.Format.String
import Haskus.Utils.Types.Generics (Generic)
import Haskus.Utils.Flow

-- | struct utsname
data SystemInfo = SystemInfo
   { systemName     :: CStringBuffer 65 -- ^ OS name
   , systemNodeName :: CStringBuffer 65 -- ^ Network name
   , systemRelease  :: CStringBuffer 65 -- ^ Release
   , systemVersion  :: CStringBuffer 65 -- ^ Version
   , systemMachine  :: CStringBuffer 65 -- ^ Hardware identifier
   } deriving (Show,Generic,Storable)

-- | "uname" syscall
systemInfo :: MonadInIO m => Flow m '[SystemInfo,ErrorCode]
systemInfo = alloca $ \(ptr :: Ptr SystemInfo) -> liftIO (syscall_uname (castPtr ptr))
      ||>   toErrorCode
      >.~.> (const (peek ptr))
