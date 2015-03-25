-- | System
module ViperVM.Arch.Linux.System.System
   ( System(..)
   , systemInit
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.System.SysFS
import ViperVM.Arch.Linux.FileSystem.Mount
import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.X86_64.Linux.FileSystem.Directory
import ViperVM.Arch.X86_64.Linux.FileSystem.Mount

import System.FilePath
import Control.Applicative ((<$>))
import Control.Monad.Trans.Either

data System = System
   { systemDevFS  :: FileDescriptor    -- ^ root of the tmpfs used to create device nodes
   , systemSysFS  :: SysFS             -- ^ systemfs
   }


-- | Create a system object
--
-- Create the given @path@ if it doesn't exist and mount the system in it
systemInit :: FilePath -> SysRet System
systemInit path = runEitherT $ do

   let 
      createDir p = EitherT $ sysCreateDirectory Nothing p [PermUserRead,PermUserWrite,PermUserExecute] False
      systemPath = path </> "sys"
      devicePath = path </> "dev"

   -- create root path and mount a tmpfs in it
   createDir path
   EitherT $ mountTmpFS sysMount path

   -- mount sysfs
   createDir systemPath
   EitherT $ mountSysFS sysMount systemPath
   sysfs <- EitherT $ fmap SysFS <$> sysOpen systemPath [OpenReadOnly] []

   -- create device directory
   createDir devicePath
   devfd <- EitherT $ sysOpen devicePath [OpenReadWrite] []

   return (System devfd sysfs)
