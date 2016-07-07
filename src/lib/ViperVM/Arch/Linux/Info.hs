{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | System info (uname)
module ViperVM.Arch.Linux.Info
   ( SystemInfo(..)
   , systemInfo
   )
where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.Storable
import Foreign.CStorable
import GHC.Generics

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.Binary.Vector
import ViperVM.Format.Binary.Word

-- | struct utsname
data SystemInfo = SystemInfo
   { systemName     :: Vector 65 CChar
   , systemNodeName :: Vector 65 CChar
   , systemRelease  :: Vector 65 CChar
   , systemVersion  :: Vector 65 CChar
   , systemMachine  :: Vector 65 CChar
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
