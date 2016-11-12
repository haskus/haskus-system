{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module ViperVM.Arch.Linux.Memory
   ( sysBrk
   , sysBrkGet
   , sysBrkSet
   , sysMemMap
   , MemProtectFlag(..)
   , MemProtectFlags
   , MapFlag(..)
   , sysMemUnmap
   , sysMemProtect
   , sysMemAdvise
   , sysMemSync
   , sysMemInCore
   , MemLockFlag(..)
   , MemLockFlags
   , sysMemLock
   , sysMemLockAll
   , sysMemUnlock
   , sysMemUnlockAll
   )
where

import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Bits ((.&.))
import ViperVM.Utils.Types
import ViperVM.Utils.Maybe (fromMaybe)
import ViperVM.Utils.Flow
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Syscalls

-- | Set program break location (i.e. data segement size)
-- 
-- On failure, Linux returns the current value. Failures include a value
-- inferior to the end of the data segment, mmaped regions already existing in the
-- region we allocate, etc.  
-- On success, the returned value is the new program break address (i.e. the given parameter).
sysBrk :: MonadIO m => Word64 -> m Word64
sysBrk addr = fromIntegral <$> liftIO (syscall @"brk" addr)

-- | Return current program break
-- We call sysBrk with an invalid value
sysBrkGet :: MonadIO m => m Word64
sysBrkGet = sysBrk 0

-- | Try to set program break and returns True on success
sysBrkSet :: MonadIO m => Word64 -> m Bool
sysBrkSet addr = (==addr) <$> sysBrk addr

data MemProtectFlag
   = ProtExec
   | ProtRead
   | ProtWrite
   | ProtSem
   | ProtGrowsDown
   | ProtGrowsUp
   deriving (Eq,Show,Enum)

instance CBitSet MemProtectFlag where
   toBitOffset x = case x of
      ProtExec       -> 2
      ProtRead       -> 0
      ProtWrite      -> 1
      ProtSem        -> 3
      ProtGrowsDown  -> 24
      ProtGrowsUp    -> 25
   fromBitOffset x = case x of
      2  -> ProtExec     
      0  -> ProtRead     
      1  -> ProtWrite    
      3  -> ProtSem      
      24 -> ProtGrowsDown
      25 -> ProtGrowsUp  
      _ -> error "Invalid memory protection flag"

type MemProtectFlags = BitSet Int64 MemProtectFlag

data MapFlag
   = MapShared
   | MapPrivate
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
   | MapHugeTLB
   deriving (Eq,Show)

instance CBitSet MapFlag where
   toBitOffset x = case x of
      MapShared         -> 0
      MapPrivate        -> 1
      MapFixed          -> 4
      MapAnonymous      -> 5
      MapUninitialized  -> 26
      MapGrowsDown      -> 8
      MapDenyWrite      -> 11
      MapExecutable     -> 12
      MapLocked         -> 13
      MapNoReserve      -> 14
      MapPopulate       -> 15
      MapNonBlock       -> 16
      MapStack          -> 17
      MapHugeTLB        -> 18
   fromBitOffset x = case x of
      0  -> MapShared
      1  -> MapPrivate
      4  -> MapFixed
      5  -> MapAnonymous
      26 -> MapUninitialized
      8  -> MapGrowsDown
      11 -> MapDenyWrite
      12 -> MapExecutable
      13 -> MapLocked
      14 -> MapNoReserve
      15 -> MapPopulate
      16 -> MapNonBlock
      17 -> MapStack
      18 -> MapHugeTLB
      _  -> error "Invalid map flag bit offset"

type MapFlagField = BitFields Word32
   '[ BitField 6  "HugeTLBSize" Word8                   -- Log2 of the huge page size
    , BitField 26 "MapFlags"    (BitSet Word32 MapFlag) -- Flags
    ]

type MapFlags = BitSet Word32 MapFlag

-- | Map files or devices into memory
--
-- Optional `hugepagesize` is in Log2 and on 6 bits
sysMemMap :: MonadIO m => Maybe (Ptr ()) -> Word64 -> MemProtectFlags -> MapFlags -> Maybe Word8 -> Maybe (Handle, Word64) -> Flow m '[Ptr (), ErrorCode]
sysMemMap addr len prot flags hugepagesize source = do
   let 
      (fd,off) = fromMaybe (-1,0) ((\(Handle fd', x) -> (fd',x)) <$> source)
      flags'   = case hugepagesize of
                  Nothing -> flags
                  Just _  -> BitSet.union flags (BitSet.fromList [MapHugeTLB])
      fld      :: MapFlagField
      fld      = updateField (Proxy :: Proxy "MapFlags") flags'
               $ updateField (Proxy :: Proxy "HugeTLBSize") (fromMaybe 0 hugepagesize)
               $ BitFields 0
      fld'     = fromIntegral (bitFieldsBits fld)
      prot'    = BitSet.toBits prot
      addr'    = fromMaybe nullPtr addr
   
   liftIO (syscall @"mmap" addr' len prot' fld' fd off)
      ||> toErrorCodePure (wordPtrToPtr . fromIntegral)

-- | Unmap memory
sysMemUnmap :: MonadIO m => Ptr () -> Word64 -> Flow m '[(),ErrorCode]
sysMemUnmap addr len = liftIO (syscall @"munmap" addr len)
   ||> toErrorCodeVoid

-- | Set protection of a region of memory
sysMemProtect :: MonadIO m => Ptr () -> Word64 -> MemProtectFlags -> Flow m '[(),ErrorCode]
sysMemProtect addr len prot = do
   let prot' = BitSet.toBits prot
   liftIO (syscall @"mprotect" addr len prot')
      ||> toErrorCodeVoid


data MemAdvice
   = MemAdviceNormal
   | MemAdviceRandom
   | MemAdviceSequential
   | MemAdviceWillNeed
   | MemAdviceDontNeed
   | MemAdviceRemove
   | MemAdviceDontFork
   | MemAdviceDoFork
   | MemAdviceHwPoison
   | MemAdviceSoftOffline
   | MemAdviceMergeable
   | MemAdviceUnmergeable
   | MemAdviceHugePage
   | MemAdviceNoHugePage
   | MemAdviceDontDump
   | MemAdviceDoDump
   deriving (Show,Eq)

instance Enum MemAdvice where
   fromEnum x = case x of
      MemAdviceNormal      -> 0
      MemAdviceRandom      -> 1
      MemAdviceSequential  -> 2
      MemAdviceWillNeed    -> 3
      MemAdviceDontNeed    -> 4
      MemAdviceRemove      -> 9
      MemAdviceDontFork    -> 10
      MemAdviceDoFork      -> 11
      MemAdviceHwPoison    -> 100
      MemAdviceSoftOffline -> 101
      MemAdviceMergeable   -> 12
      MemAdviceUnmergeable -> 13
      MemAdviceHugePage    -> 14
      MemAdviceNoHugePage  -> 15
      MemAdviceDontDump    -> 16
      MemAdviceDoDump      -> 17

   toEnum x = case x of
      0   -> MemAdviceNormal      
      1   -> MemAdviceRandom      
      2   -> MemAdviceSequential  
      3   -> MemAdviceWillNeed    
      4   -> MemAdviceDontNeed    
      9   -> MemAdviceRemove      
      10  -> MemAdviceDontFork    
      11  -> MemAdviceDoFork      
      100 -> MemAdviceHwPoison    
      101 -> MemAdviceSoftOffline 
      12  -> MemAdviceMergeable   
      13  -> MemAdviceUnmergeable 
      14  -> MemAdviceHugePage    
      15  -> MemAdviceNoHugePage  
      16  -> MemAdviceDontDump    
      17  -> MemAdviceDoDump      
      _   -> error "Unknown mem advice code"


sysMemAdvise :: MonadIO m => Ptr () -> Word64 -> MemAdvice -> Flow m '[(),ErrorCode]
sysMemAdvise addr len adv = 
   liftIO (syscall @"madvise" addr len (fromEnum adv))
      ||> toErrorCodeVoid

data MemSync
   = MemAsync
   | MemInvalidate
   | MemSync
   deriving (Show,Eq,Enum,CBitSet)

type MemSyncFlags = BitSet Word32 MemSync

sysMemSync :: MonadIO m => Ptr () -> Word64 -> MemSyncFlags -> Flow m '[(),ErrorCode]
sysMemSync addr len flag = 
   liftIO (syscall @"msync" addr len (fromIntegral (BitSet.toBits flag)))
      ||> toErrorCodeVoid

sysMemInCore :: MonadInIO m => Ptr () -> Word64 -> Flow m '[[Bool],ErrorCode]
sysMemInCore addr len = do
   -- On x86-64, page size is at least 4k
   let n = fromIntegral $ (len + 4095) `div` 4096
   allocaArray n $ \arr ->
      liftIO (syscall @"mincore" addr len (arr :: Ptr Word8))
         ||>   toErrorCode
         >.~.> (const (fmap (\x -> x .&. 1 == 1) <$> peekArray n arr))


sysMemLock :: MonadIO m => Ptr () -> Word64 -> Flow m '[(),ErrorCode]
sysMemLock addr len = liftIO (syscall @"mlock" addr len)
   ||> toErrorCodeVoid

sysMemUnlock :: MonadIO m => Ptr () -> Word64 -> Flow m '[(),ErrorCode]
sysMemUnlock addr len = liftIO (syscall @"munlock" addr len)
   ||> toErrorCodeVoid

data MemLockFlag
   = LockCurrentPages
   | LockFuturePages
   deriving (Show,Eq,Enum,CBitSet)

type MemLockFlags = BitSet Word64 MemLockFlag

sysMemLockAll :: MonadIO m => MemLockFlags -> Flow m '[(),ErrorCode]
sysMemLockAll flags = liftIO (syscall @"mlockall" (BitSet.toBits flags))
   ||> toErrorCodeVoid

sysMemUnlockAll :: MonadIO m => Flow m '[(),ErrorCode]
sysMemUnlockAll = liftIO (syscall @"munlockall")
   ||> toErrorCodeVoid
