-- | System
module ViperVM.Arch.Linux.System.System
   ( System(..)
   , systemInit
   )
where

import qualified ViperVM.Utils.BitSet as BitSet
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.System.SysFS
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.Directory
import ViperVM.Arch.Linux.FileSystem.Mount

import System.FilePath
import Control.Monad (void)

data System = System
   { systemDevFS  :: FileDescriptor    -- ^ root of the tmpfs used to create device nodes
   , systemSysFS  :: SysFS             -- ^ systemfs
   }


-- | Create a system object
--
-- Create the given @path@ if it doesn't exist and mount the system in it
systemInit :: FilePath -> SysRet System
systemInit path = do

   let 
      createDir p = sysCreateDirectory Nothing p (BitSet.fromList [PermUserRead,PermUserWrite,PermUserExecute]) False
      systemPath = path </> "sys"
      devicePath = path </> "dev"

   -- create root path (allowed to fail if it already exists)
   void $ runCatch $ sysTry "Create root directory" $ createDir path

   runCatch $ do
      -- mount a tmpfs in root path
      sysTry "Mount tmpfs" $ mountTmpFS sysMount path

      -- mount sysfs
      sysTry "Create system directory" $ createDir systemPath
      sysTry "Mount sysfs" $ mountSysFS sysMount systemPath
      sysfs <- sysTry "Open sysfs directory" $ sysOpen systemPath [OpenReadOnly] BitSet.empty

      -- create device directory
      sysTry "Create device directory" $ createDir devicePath
      devfd <- sysTry "Open device directory" $ sysOpen devicePath [OpenReadOnly] BitSet.empty

      return (System devfd (SysFS sysfs))
