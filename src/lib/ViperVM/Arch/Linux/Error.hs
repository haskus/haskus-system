
-- | Linux error management
module ViperVM.Arch.Linux.Error
   ( RetryLater (..)
   , Overflow (..)
   , InvalidHandle (..)
   , Interrupted (..)
   , NotAllowed (..)
   , InvalidRestartCommand (..)
   , MemoryError (..)
   , InvalidParam (..)
   , EntryNotFound (..)
   , DeviceNotFound (..)
   , InvalidRange (..)
   , FileSystemIOError (..)
   , OutOfKernelMemory (..)
   -- ** Path errors
   , SymbolicLinkLoop (..)
   , NotSymbolicLink (..)
   , TooLongPathName (..)
   , InvalidPathComponent (..)
   , FileNotFound (..)
   , InvalidIsDirectory (..)
   )
where

import ViperVM.Arch.Linux.Internals.Handle (Handle)

------------------------------------------------
-- Errors
------------------------------------------------

-- | Not allowed
data NotAllowed
   = NotAllowed
   deriving (Show,Eq)

-- | Overflow
data Overflow
   = Overflow
   deriving (Show,Eq)

-- | Interrupted
data Interrupted
   = Interrupted
   deriving (Show,Eq)

-- | Invalid handle error
data InvalidHandle
   = InvalidHandle Handle
   deriving (Show,Eq)

-- | Invalid restart commmand
data InvalidRestartCommand
   = InvalidRestartCommand
   deriving (Show,Eq)

-- | Memory error
data MemoryError
   = MemoryError
   deriving (Show,Eq)

-- | Invalid parameter
data InvalidParam
   = InvalidParam
   deriving (Show,Eq)

-- | Entry not found
data EntryNotFound
   = EntryNotFound
   deriving (Show,Eq)

-- | File not found
data FileNotFound
   = FileNotFound
   deriving (Show,Eq)

-- | Invalid directory handle
data InvalidIsDirectory
   = InvalidIsDirectory
   deriving (Show,Eq)

-- | Device not found
data DeviceNotFound
   = DeviceNotFound
   deriving (Show,Eq)

-- | Retry later
data RetryLater
   = RetryLater
   deriving (Show,Eq)

-- | Invalid range
data InvalidRange
   = InvalidRange
   deriving (Show,Eq)

-- | Not a symbolic link
data NotSymbolicLink
   = NotSymbolicLink
   deriving (Show,Eq)

-- | FileSystem I/O error
data FileSystemIOError
   = FileSystemIOError
   deriving (Show,Eq)

-- | Too many symbolic links in a path (possible symbolic link loop)
data SymbolicLinkLoop
   = SymbolicLinkLoop
   deriving (Show,Eq)

-- | A pathname, or a component of a athname, is too long
data TooLongPathName
   = TooLongPathName
   deriving (Show,Eq)

-- | Kernel memory error
data OutOfKernelMemory
   = OutOfKernelMemory
   deriving (Show,Eq)

-- | A component of a path is not a directory
data InvalidPathComponent
   = InvalidPathComponent
   deriving (Show,Eq)
