module ViperVM.Arch.X86_64.Linux.Memory (
   sysBrk, sysBrkGet, sysBrkSet, sysMemMap,
   MemProtect(..), MapFlag(..), sysMemUnmap, sysMemProtect
) where

import Data.Word (Word8,Word64)
import Data.Int (Int64)
import Control.Applicative ((<$>))
import Foreign.Ptr (Ptr, nullPtr, intPtrToPtr)
import Data.Maybe (fromMaybe)
import Data.Bits ((.|.), (.&.), shiftL)

import ViperVM.Arch.X86_64.Linux.Syscall
import ViperVM.Arch.X86_64.Linux.ErrorCode
import ViperVM.Arch.X86_64.Linux.FileSystem (FileDescriptor(..))

-- | Set program break location (i.e. data segement size)
-- 
-- On failure, Linux returns the current value. Failures include a value
-- inferior to the end of the data segment, mmaped regions already existing in the
-- region we allocate, etc.  
-- On success, the returned value is the new program break address (i.e. the given parameter).
sysBrk :: Word64 -> IO Word64
sysBrk addr = fromIntegral <$> syscall1 12 addr

-- | Return current program break
-- We call sysBrk with an invalid value
sysBrkGet :: IO Word64
sysBrkGet = sysBrk 0

-- | Try to set program break and returns True on success
sysBrkSet :: Word64 -> IO Bool
sysBrkSet addr = (==addr) <$> sysBrk addr

data MemProtect =
     ProtExec
   | ProtRead
   | ProtWrite
   | ProtSem
   | ProtGrowsDown
   | ProtGrowsUp
   deriving (Eq,Show)

instance Enum MemProtect where
   fromEnum x = case x of
      ProtExec       -> 0x04
      ProtRead       -> 0x01
      ProtWrite      -> 0x02
      ProtSem        -> 0x08
      ProtGrowsDown  -> 0x01000000
      ProtGrowsUp    -> 0x02000000

   toEnum x = case x of
      0x04       -> ProtExec
      0x01       -> ProtRead
      0x02       -> ProtWrite
      0x08       -> ProtSem
      0x01000000 -> ProtGrowsDown
      0x02000000 -> ProtGrowsUp
      _ -> error "Invalid flag"


data MapFlag =
     MapShared
   | MapPrivate
   | MapType
   | MapFixed
   | MapAnonymous
   | MapUninitialized
   | MapGrowsDown
   | MapDenyWrite
   | MapExecutable
   | MapLocked
   | MapNoReserve
   | MapPopulate
   | MapNonBlock
   | MapStack
   | MapHugeTLB Word8   -- ^ Page size (6 bits)
   deriving (Eq,Show)

instance Enum MapFlag where
   fromEnum x = case x of
      MapShared         -> 0x01
      MapPrivate        -> 0x02
      MapType           -> 0x0F
      MapFixed          -> 0x10
      MapAnonymous      -> 0x20
      MapUninitialized  -> 0x4000000
      MapGrowsDown      -> 0x0100
      MapDenyWrite      -> 0x0800
      MapExecutable     -> 0x1000
      MapLocked         -> 0x2000
      MapNoReserve      -> 0x4000
      MapPopulate       -> 0x8000
      MapNonBlock       -> 0x10000
      MapStack          -> 0x20000
      MapHugeTLB sz 
         | sz .&. 0xC0 == 0 -> 0x40000 .|. (fromIntegral sz `shiftL` 26)
         | otherwise        -> error "Page size too big"

   toEnum = undefined

-- | Map files or devices into memory
sysMemMap :: Maybe (Ptr ()) -> Word64 -> [MemProtect] -> [MapFlag] -> Maybe (FileDescriptor, Word64) -> SysRet (Ptr ())
sysMemMap addr len prot flags source = do
   let 
      (fd,off) = fromMaybe (-1,0) ((\(FileDescriptor fd', x) -> (fd',x)) <$> source)
      flags' = toSet flags :: Int64
      prot' = toSet prot :: Int64
      addr' = fromMaybe nullPtr addr
   
   onSuccess (syscall6 9 addr' len prot' flags' fd off) (intPtrToPtr . fromIntegral)

-- | Unmap memory
sysMemUnmap :: Ptr () -> Word64 -> SysRet ()
sysMemUnmap addr len =
   onSuccess (syscall2 11 addr len) (const ())

-- | Set protection of a region of memory
sysMemProtect :: Ptr () -> Word64 -> [MemProtect] -> SysRet ()
sysMemProtect addr len prot = do
   let prot' = toSet prot :: Int64
   onSuccess (syscall3 10 addr len prot') (const ())
