{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Linux syscall
module Haskus.Arch.X86_64.Linux.Syscall
   ( S
   , PrimOp
   , Safe
   , SyscallByName
   , syscall_
   , SelSyscall
   -- * Internals
   , syscall0primop
   , syscall1primop
   , syscall2primop
   , syscall3primop
   , syscall4primop
   , syscall5primop
   , syscall6primop
   , syscall0safe
   , syscall1safe
   , syscall2safe
   , syscall3safe
   , syscall4safe
   , syscall5safe
   , syscall6safe
   )
where

import Haskus.Arch.Linux.Internals.Arg
import Haskus.Format.Binary.Word
import Haskus.Utils.Types
import GHC.Base
import GHC.Int

------------------------------------
-- Syscalls
-- ~~~~~~~~
--
-- We support syscall definition as a type-level table:
--
--    type Syscalls =
--       '[ S 16 PrimOp "ioctl" (Int64 -> Int64 -> IO Int64)
--        , S 17 Safe   "dummy"  (IO Int64)
--        ]
--
-- For each syscall (S n s name t):
--    - n is the syscall number
--    - s is the safety: PrimOp or Safe (FFI)
--    - name is the syscall name
--    - t is the syscall parameters and return types
--
-- To call a syscall, use 'syscall_' as follow:
--    syscall_ @Syscalls @"ioctl"
--
-- A simple wrapper should be defined alongside each table:
--    syscall = syscall_ @Syscalls
--
-- Why do we do this?
-- ------------------
--
-- 1) It avoids defining one function per syscall
-- 2) It makes it easier to read and check against the kernel tables
-- 3) It makes it easier to switch from one table to another on another
-- architecture
-- 4) It allows syscall numbers to be retrieved by name (mayb be useful to
-- generate code)
--

-- | syscall with the given name from the given syscall table
syscall_ :: forall (syscalls :: [*]) (name :: Symbol) (n :: Nat) s t.
   ( S n s name t ~ SyscallByName name syscalls
   , SelSyscall s t
   , KnownNat n
   ) => t
{-# INLINE syscall_ #-}
syscall_ = (selectCall @s :: Int64 -> t) (natValue @n)

data S (n :: Nat) safety (name :: Symbol) t

type family SyscallByName (name :: Symbol) (xs :: [*]) where
   SyscallByName name '[]                 = TypeError ('Text "Cannot find syscall " ':<>: 'ShowType name)
   SyscallByName name (S n s name t : xs) = S n s name t
   SyscallByName name (S n s xxxx t : xs) = SyscallByName name xs

-- | Call syscall using primop
data PrimOp

-- | Call syscall using safe FFI
data Safe

-- | Select the syscall function
class SelSyscall s t where
   selectCall :: Int64 -> t

instance SelSyscall PrimOp (IO Int64) where
   selectCall               = syscall0primop

instance Arg a => SelSyscall PrimOp (a -> IO Int64) where
   selectCall n a           = syscall1primop n (toArg a)

instance (Arg a, Arg b) => SelSyscall PrimOp (a -> b -> IO Int64) where
   selectCall n a b         = syscall2primop n (toArg a) (toArg b)

instance (Arg a, Arg b, Arg c) => SelSyscall PrimOp (a -> b -> c -> IO Int64) where
   selectCall n a b c       = syscall3primop n (toArg a) (toArg b) (toArg c)

instance (Arg a, Arg b, Arg c, Arg d) => SelSyscall PrimOp (a -> b -> c -> d -> IO Int64) where
   selectCall n a b c d     = syscall4primop n (toArg a) (toArg b) (toArg c) (toArg d)

instance (Arg a, Arg b, Arg c, Arg d, Arg e) => SelSyscall PrimOp (a -> b -> c -> d -> e -> IO Int64) where
   selectCall n a b c d e   = syscall5primop n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => SelSyscall PrimOp (a -> b -> c -> d -> e -> f -> IO Int64) where
   selectCall n a b c d e f = syscall6primop n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

instance SelSyscall Safe (IO Int64) where
   selectCall               = syscall0safe

instance Arg a => SelSyscall Safe (a -> IO Int64) where
   selectCall n a           = syscall1safe n (toArg a)

instance (Arg a, Arg b) => SelSyscall Safe (a -> b -> IO Int64) where
   selectCall n a b         = syscall2safe n (toArg a) (toArg b)

instance (Arg a, Arg b, Arg c) => SelSyscall Safe (a -> b -> c -> IO Int64) where
   selectCall n a b c       = syscall3safe n (toArg a) (toArg b) (toArg c)

instance (Arg a, Arg b, Arg c, Arg d) => SelSyscall Safe (a -> b -> c -> d -> IO Int64) where
   selectCall n a b c d     = syscall4safe n (toArg a) (toArg b) (toArg c) (toArg d)

instance (Arg a, Arg b, Arg c, Arg d, Arg e) => SelSyscall Safe (a -> b -> c -> d -> e -> IO Int64) where
   selectCall n a b c d e   = syscall5safe n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => SelSyscall Safe (a -> b -> c -> d -> e -> f -> IO Int64) where
   selectCall n a b c d e f = syscall6safe n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)


--------------------------------------------------
-- Implementation using Haskell foreign primops
--------------------------------------------------

foreign import prim "x86_64_linux_syscall_primop6" syscall6_# :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop5" syscall5_# :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop4" syscall4_# :: Int# -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop3" syscall3_# :: Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop2" syscall2_# :: Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop1" syscall1_# :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop0" syscall0_# :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)

-- | Syscall with 6 parameters
syscall6primop :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall6primop (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) (I64# f) = IO $ \s ->
   case (syscall6_# n a b c d e f s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 5 parameters
syscall5primop :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall5primop (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) = IO $ \s ->
   case (syscall5_# n a b c d e s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 4 parameters
syscall4primop :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall4primop (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) = IO $ \s ->
   case (syscall4_# n a b c d s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 3 parameters
syscall3primop :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall3primop (I64# n) (I64# a) (I64# b) (I64# c) = IO $ \s ->
   case (syscall3_# n a b c s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 2 parameters
syscall2primop :: Int64 -> Int64 -> Int64 -> IO Int64
syscall2primop (I64# n) (I64# a) (I64# b) = IO $ \s ->
   case (syscall2_# n a b s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 1 parameter
syscall1primop :: Int64 -> Int64 -> IO Int64
syscall1primop (I64# n) (I64# a) = IO $ \s ->
   case (syscall1_# n a s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 0 parameter
syscall0primop :: Int64 -> IO Int64
syscall0primop (I64# n) = IO $ \s ->
   case (syscall0_# n s) of (# s1, r #) -> (# s1, I64# r #)

--------------------------------------------------
-- Implementation using Haskell FFI
--------------------------------------------------


foreign import ccall safe "x86_64_linux_syscall6" syscall6safe :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall5" syscall5safe :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall4" syscall4safe :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall3" syscall3safe :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall2" syscall2safe :: Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall1" syscall1safe :: Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall0" syscall0safe :: Int64 -> IO Int64
