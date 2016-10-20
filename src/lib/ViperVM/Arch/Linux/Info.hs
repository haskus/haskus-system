{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | System info (uname)
module ViperVM.Arch.Linux.Info
   ( SystemInfo(..)
   , systemInfo
   )
where

import Foreign.Marshal.Alloc

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.String
import ViperVM.Utils.Types.Generics (Generic)

-- | struct utsname
data SystemInfo = SystemInfo
   { systemName     :: CStringBuffer 65 -- ^ OS name
   , systemNodeName :: CStringBuffer 65 -- ^ Network name
   , systemRelease  :: CStringBuffer 65 -- ^ Release
   , systemVersion  :: CStringBuffer 65 -- ^ Version
   , systemMachine  :: CStringBuffer 65 -- ^ Hardware identifier
   } deriving (Show,Generic,CStorable)

instance Storable SystemInfo where
   peek      = cPeek
   poke      = cPoke
   alignment = cAlignment
   sizeOf    = cSizeOf

-- | "uname" syscall
systemInfo :: SysRet SystemInfo
systemInfo = alloca $ \ptr -> onSuccessIO (uname ptr) (const (peek ptr))
   where
      uname :: Ptr SystemInfo -> IO Int64
      uname = syscall_uname . castPtr
