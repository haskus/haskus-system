module ViperVM.Arch.X86_64.Linux.Info (
   SystemInfo(..), sysSystemInfo
) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String
import Control.Monad

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode

data SystemInfo = SystemInfo {
   systemName :: String,
   systemNodeName :: String,
   systemRelease :: String,
   systemVersion :: String,
   systemMachine :: String
} deriving (Show)

-- | "uname" syscall
sysSystemInfo :: SysRet SystemInfo
sysSystemInfo = go (5 * fieldSize)
   where
      fieldSize = 65
      go sz = do
         ret <- allocaArray sz $ \ptr ->
            onSuccessIO (syscall1 63 (ptr :: CString)) $ \_ -> do
               [nam,nodeName,rel,ver,mach] <- forM [0..4] $ \n -> peekCString (ptr `plusPtr` (n*fieldSize))
               return $ SystemInfo nam nodeName rel ver mach
         case ret of
            Right _ -> return ret
            Left _  -> go (2 * sz) -- We can be sure to find a valid size
