
-- | Linux error management
module Haskus.Arch.Linux.Error
   ( RetryLater (..)
   , Overflow (..)
   , InvalidHandle (..)
   , TooManyProcessHandles (..)
   , TooManySystemHandles (..)
   , Interrupted (..)
   , NotAllowed (..)
   , ExhaustedQuota (..)
   , InvalidRestartCommand (..)
   , MemoryError (..)
   , InvalidParam (..)
   , EntryNotFound (..)
   , DeviceNotFound (..)
   , InvalidRange (..)
   , FileSystemIOError (..)
   , OutOfKernelMemory (..)
   , OutOfSpace (..)
   , TempFileNotSupported (..)
   , ReadOnlyFileSystem (..)
   , CannotWriteExecutedImage (..)
   -- ** Path errors
   , SymbolicLinkLoop (..)
   , NotSymbolicLink (..)
   , TooLongPathName (..)
   , TooManyLinks (..)
   , InvalidPathComponent (..)
   , FileNotFound (..)
   , FileAlreadyExists (..)
   , InvalidIsDirectory (..)
   , NotADirectory (..)
   , BusyDirectory (..)
   , NotEmptyDirectory (..)
   , NotTheSameFileSystem (..)
   -- ** Low-level errors
   , ErrorCode (..)
   )
where

import Haskus.Arch.Linux.Internals.Error

------------------------------------------------
-- Errors
------------------------------------------------

-- | Not allowed
data NotAllowed
   = NotAllowed
   deriving (Show,Eq)

-- | Quota exhausted
data ExhaustedQuota
   = ExhaustedQuota
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
   = InvalidHandle
   deriving (Show,Eq)

-- | Too many open handles in the process
data TooManyProcessHandles
   = TooManyProcessHandles
   deriving (Show,Eq)

-- | Too many open handles in the system
data TooManySystemHandles
   = TooManySystemHandles
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

-- | File already exists
data FileAlreadyExists
   = FileAlreadyExists
   deriving (Show,Eq)

-- | Invalid directory handle
data InvalidIsDirectory
   = InvalidIsDirectory
   deriving (Show,Eq)

-- | Invalid: not a directory handle
data NotADirectory
   = NotADirectory
   deriving (Show,Eq)

-- | Invalid: busy directory
data BusyDirectory
   = BusyDirectory
   deriving (Show,Eq)

-- | Invalid: directory not empty
data NotEmptyDirectory
   = NotEmptyDirectory
   deriving (Show,Eq)

-- | Invalid: not the same filesystem
data NotTheSameFileSystem
   = NotTheSameFileSystem
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

-- | Too many links (EMLINK)
data TooManyLinks
   = TooManyLinks
   deriving (Show,Eq)

-- | Kernel memory error
data OutOfKernelMemory
   = OutOfKernelMemory
   deriving (Show,Eq)

-- | Out of space
data OutOfSpace
   = OutOfSpace
   deriving (Show,Eq)

-- | Filesystem does not support temporary files
data TempFileNotSupported
   = TempFileNotSupported
   deriving (Show,Eq)

-- | Cannot write into an image being executed
data CannotWriteExecutedImage
   = CannotWriteExecutedImage
   deriving (Show,Eq)

-- | Read-only filesystem
data ReadOnlyFileSystem
   = ReadOnlyFileSystem
   deriving (Show,Eq)

-- | A component of a path is not a directory
data InvalidPathComponent
   = InvalidPathComponent
   deriving (Show,Eq)
