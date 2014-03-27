module ViperVM.Arch.X86_64.Linux.Process (
   ProcessID(..),
   sysExit, sysGetCPU, sysGetProcessID, sysGetParentProcessID
) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Word (Word, Word32)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Control.Applicative ((<$>), (<*>))

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode

newtype ProcessID = ProcessID Word32 deriving (Show,Eq,Ord)

-- | Exit the current process with the given return value
-- This syscall does not return.
sysExit :: Int64 -> IO ()
sysExit n = void (syscall1 60 n)

-- | Get CPU and NUMA node executing the current process
sysGetCPU :: SysRet (Word,Word)
sysGetCPU =
   alloca $ \cpu ->
      alloca $ \node -> do
         ret <- syscall3 309 (cpu :: Ptr Word) (node :: Ptr Word) nullPtr
         if ret < 0 
            then return (toLeftErrorCode ret)
            else fmap Right . (,) <$> peek cpu <*> peek node

-- | Return process ID
sysGetProcessID :: IO ProcessID
sysGetProcessID = ProcessID . fromIntegral <$> syscall0 39

-- | Return parent process ID
sysGetParentProcessID :: IO ProcessID
sysGetParentProcessID = ProcessID . fromIntegral <$> syscall0 110
