{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Process tracing (ptrace)
module Haskus.Arch.Linux.Trace
   ( TraceRequest(..)
   , TraceFlag(..)
   , TraceOption(..)
   , PeekSigInfoFlags(..)
   , PeekSigInfoArgs(..)
   , sysTrace
   )
where

import Haskus.Arch.Linux.Syscalls
import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Process (ProcessID(..))
import Haskus.Format.Binary.BitSet
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Utils.Flow

-- | Tracing request
data TraceRequest
   = ReqTraceMe          -- ^ Indicate that the process making this request should be
                         --   traced.  All signals received by this process can be intercepted by its
                         --   parent, and its parent can use the other `ptrace' requests
   | ReqPeekText         -- ^ Return the word in the process's text space at address ADDR
   | ReqPeekData         -- ^ Return the word in the process's data space at address ADDR
   | ReqPeekUser         -- ^ Return the word in the process's user area at offset ADDR
   | ReqPokeText         -- ^ Write the word DATA into the process's text space at address ADDR
   | ReqPokeData         -- ^ Write the word DATA into the process's data space at address ADDR
   | ReqPokeUser         -- ^ Write the word DATA into the process's user area at offset ADDR
   | ReqContinue         -- ^ Continue the process
   | ReqKill             -- ^ Kill the process
   | ReqSingleStep       -- ^ Single step the process. This is not supported on all machines
   | ReqGetRegisters     -- ^ Get all general purpose registers used by a processes. This is not supported on all machines
   | ReqSetRegisters     -- ^ Set all general purpose registers used by a processes. This is not supported on all machines
   | ReqGetFPRegisters   -- ^ Get all floating point registers used by a processes. This is not supported on all machines
   | ReqSetFPRegisters   -- ^ Set all floating point registers used by a processes. This is not supported on all machines
   | ReqAttach           -- ^ Attach to a process that is already running
   | ReqDetach           -- ^ Detach from a process attached to with PTRACE_ATTACH
   | ReqGetFPXRegisters  -- ^ Get all extended floating point registers used by a processes. This is not supported on all machines
   | ReqSetFPXRegisters  -- ^ Set all extended floating point registers used by a processes. This is not supported on all machines
   | ReqSyscall          -- ^ Continue and stop at the next (return from) syscall
   | ReqSetOptions       -- ^ Set ptrace filter options
   | ReqGetLastMessage   -- ^ Get last ptrace message
   | ReqGetSignalInfo    -- ^ Retrieve information about the signal that caused the stop
   | ReqSetSignalInfo    -- ^ Set signal information
   | ReqGetRegister      -- ^ Get register content
   | ReqSetRegister      -- ^ Set register content
   | ReqSeize            -- ^ Like PTRACE_ATTACH, but do not force tracee to trap and do not affect signal or group stop state
   | ReqInterrupt        -- ^ Trap seized tracee
   | ReqListen           -- ^ Wait for next group event
   | ReqPeekSignals      -- ^ Retrieve signal information
   deriving (Show,Eq)

instance Enum TraceRequest where
   fromEnum x = case x of
      ReqTraceMe         -> 0 
      ReqPeekText        -> 1
      ReqPeekData        -> 2
      ReqPeekUser        -> 3
      ReqPokeText        -> 4
      ReqPokeData        -> 5
      ReqPokeUser        -> 6
      ReqContinue        -> 7
      ReqKill            -> 8
      ReqSingleStep      -> 9
      ReqGetRegisters    -> 12
      ReqSetRegisters    -> 13
      ReqGetFPRegisters  -> 14
      ReqSetFPRegisters  -> 15
      ReqAttach          -> 16
      ReqDetach          -> 17
      ReqGetFPXRegisters -> 18
      ReqSetFPXRegisters -> 19
      ReqSyscall         -> 24
      ReqSetOptions      -> 0x4200
      ReqGetLastMessage  -> 0x4201
      ReqGetSignalInfo   -> 0x4202
      ReqSetSignalInfo   -> 0x4203
      ReqGetRegister     -> 0x4204
      ReqSetRegister     -> 0x4205
      ReqSeize           -> 0x4206
      ReqInterrupt       -> 0x4207
      ReqListen          -> 0x4208
      ReqPeekSignals     -> 0x4209
   toEnum x = case x of
      0        -> ReqTraceMe
      1        -> ReqPeekText
      2        -> ReqPeekData
      3        -> ReqPeekUser
      4        -> ReqPokeText
      5        -> ReqPokeData
      6        -> ReqPokeUser
      7        -> ReqContinue
      8        -> ReqKill
      9        -> ReqSingleStep
      12       -> ReqGetRegisters
      13       -> ReqSetRegisters
      14       -> ReqGetFPRegisters
      15       -> ReqSetFPRegisters
      16       -> ReqAttach
      17       -> ReqDetach
      18       -> ReqGetFPXRegisters
      19       -> ReqSetFPXRegisters
      24       -> ReqSyscall
      0x4200   -> ReqSetOptions
      0x4201   -> ReqGetLastMessage
      0x4202   -> ReqGetSignalInfo
      0x4203   -> ReqSetSignalInfo
      0x4204   -> ReqGetRegister
      0x4205   -> ReqSetRegister
      0x4206   -> ReqSeize
      0x4207   -> ReqInterrupt
      0x4208   -> ReqListen
      0x4209   -> ReqPeekSignals
      _        -> error "Invalid request"

-- | Tracing flag
data TraceFlag
   = FlagSeizeDevel

instance Enum TraceFlag where
   fromEnum FlagSeizeDevel = 0x80000000
   toEnum 0x80000000 = FlagSeizeDevel
   toEnum _          = error "Invalid trace flag"

-- | Tracing options
data TraceOption
   = OptFlagSyscallTrap -- ^ When delivering system call traps set bit 7 in the signal number
   | OptTraceFork       -- ^ Stop the tracee at the next fork and automatically start tracing the newly forked process
   | OptTraceVFork      -- ^ Stop the tracee at the next fork and automatically start tracing the newly vforked process
   | OptTraceClone      -- ^ Stop the tracee at the next fork and automatically start tracing the newly cloned process
   | OptTraceExec       -- ^ Stop the tracee at the next execve
   | OptTraceVForkDone  -- ^ Stop the tracee at the completion of the next vfork
   | OptTraceExit       -- ^ Stop the tracee at exit
   | OptTraceSecComp    -- ^ Stop the tracee when a seccomp SECCOMP_RET_TRACE rule is triggered
   | OptTraceExitKill   -- ^ When set, tracee processes will receive a KILL signal when the tracer process exits
   deriving (Eq,Show,Enum)

instance CBitSet TraceOption where
   toBitOffset x = case x of
      OptTraceExitKill   -> 20
      _                  -> fromEnum x
   fromBitOffset x = case x of
      20 -> OptTraceExitKill
      _  -> toEnum x

-- | Event code
data EventCode
   = EventFork
   | EventVFork
   | EventClone
   | EventExec
   | EventVForkDone
   | EventExit
   | EventSecComp
   deriving (Show,Eq)

instance Enum EventCode where
   fromEnum x = case x of
      EventFork      -> 1
      EventVFork     -> 2
      EventClone     -> 3
      EventExec      -> 4
      EventVForkDone -> 5
      EventExit      -> 6
      EventSecComp   -> 7
   toEnum x = case x of
      1 -> EventFork
      2 -> EventVFork
      3 -> EventClone
      4 -> EventExec
      5 -> EventVForkDone
      6 -> EventExit
      7 -> EventSecComp
      _ -> error "Invalid event code"

-- | Peek signals
data PeekSigInfoArgs = PeekSigInfoArgs
   { peekSigOffset :: Word64
   , peekSigFlags  :: Word32
   , peekSigCount  :: Int32
   }

-- | Peek signals flags
data PeekSigInfoFlags
   = PeekSigInfoShared
   deriving (Enum,CBitSet)
   
   
-- | Trace a process
sysTrace :: MonadIO m => TraceRequest -> ProcessID -> Ptr () -> Ptr () -> Flow m '[Int64,ErrorCode]
sysTrace req (ProcessID pid) addr dat =
   liftIO (syscall @"ptrace" (fromEnum req) pid addr dat)
      ||> toErrorCode
