{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Linux syscall
module ViperVM.Arch.X86_64.Linux.Syscall
   ( syscall0
   , syscall1
   , syscall2
   , syscall3
   , syscall4
   , syscall5
   , syscall6
   , syscall0safe
   , syscall1safe
   , syscall2safe
   , syscall3safe
   , syscall4safe
   , syscall5safe
   , syscall6safe
   )
where

import ViperVM.Arch.Linux.Internals.Arg
import ViperVM.Format.Binary.Word
import GHC.Base
import GHC.Int

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
syscall6_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall6_ (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) (I64# f) = IO $ \s ->
   case (syscall6_# n a b c d e f s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 5 parameters
syscall5_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall5_ (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) = IO $ \s ->
   case (syscall5_# n a b c d e s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 4 parameters
syscall4_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall4_ (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) = IO $ \s ->
   case (syscall4_# n a b c d s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 3 parameters
syscall3_ :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall3_ (I64# n) (I64# a) (I64# b) (I64# c) = IO $ \s ->
   case (syscall3_# n a b c s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 2 parameters
syscall2_ :: Int64 -> Int64 -> Int64 -> IO Int64
syscall2_ (I64# n) (I64# a) (I64# b) = IO $ \s ->
   case (syscall2_# n a b s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 1 parameter
syscall1_ :: Int64 -> Int64 -> IO Int64
syscall1_ (I64# n) (I64# a) = IO $ \s ->
   case (syscall1_# n a s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 0 parameter
syscall0_ :: Int64 -> IO Int64
syscall0_ (I64# n) = IO $ \s ->
   case (syscall0_# n s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 6 parameters
syscall6 :: (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Int64 -> a -> b -> c -> d -> e -> f -> IO Int64
syscall6 n a b c d e f = syscall6_ n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

-- | Syscall with 5 parameters
syscall5 :: (Arg a, Arg b, Arg c, Arg d, Arg e) => Int64 -> a -> b -> c -> d -> e -> IO Int64
syscall5 n a b c d e = syscall5_ n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

-- | Syscall with 4 parameters
syscall4 :: (Arg a, Arg b, Arg c, Arg d) => Int64 -> a -> b -> c -> d -> IO Int64
syscall4 n a b c d = syscall4_ n (toArg a) (toArg b) (toArg c) (toArg d)

-- | Syscall with 3 parameters
syscall3 :: (Arg a, Arg b, Arg c) => Int64 -> a -> b -> c -> IO Int64
syscall3 n a b c = syscall3_ n (toArg a) (toArg b) (toArg c)

-- | Syscall with 2 parameters
syscall2 :: (Arg a, Arg b) => Int64 -> a -> b -> IO Int64
syscall2 n a b = syscall2_ n (toArg a) (toArg b)

-- | Syscall with 1 parameter
syscall1 :: (Arg a) => Int64 -> a -> IO Int64
syscall1 n a = syscall1_ n (toArg a)

-- | Syscall with 0 parameter
syscall0 :: Int64 -> IO Int64
syscall0 = syscall0_




--------------------------------------------------
-- Implementation using Haskell FFI
--------------------------------------------------


foreign import ccall safe "x86_64_linux_syscall6" syscall6safe_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall5" syscall5safe_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall4" syscall4safe_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall3" syscall3safe_ :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall2" syscall2safe_ :: Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall1" syscall1safe_ :: Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall0" syscall0safe_ :: Int64 -> IO Int64

-- | Syscall with 6 parameters
syscall6safe :: (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Int64 -> a -> b -> c -> d -> e -> f -> IO Int64
syscall6safe n a b c d e f = syscall6safe_ n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

-- | Syscall with 5 parameters
syscall5safe :: (Arg a, Arg b, Arg c, Arg d, Arg e) => Int64 -> a -> b -> c -> d -> e -> IO Int64
syscall5safe n a b c d e = syscall5safe_ n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

-- | Syscall with 4 parameters
syscall4safe :: (Arg a, Arg b, Arg c, Arg d) => Int64 -> a -> b -> c -> d -> IO Int64
syscall4safe n a b c d = syscall4safe_ n (toArg a) (toArg b) (toArg c) (toArg d)

-- | Syscall with 3 parameters
syscall3safe :: (Arg a, Arg b, Arg c) => Int64 -> a -> b -> c -> IO Int64
syscall3safe n a b c = syscall3safe_ n (toArg a) (toArg b) (toArg c)

-- | Syscall with 2 parameters
syscall2safe :: (Arg a, Arg b) => Int64 -> a -> b -> IO Int64
syscall2safe n a b = syscall2safe_ n (toArg a) (toArg b)

-- | Syscall with 1 parameter
syscall1safe :: (Arg a) => Int64 -> a -> IO Int64
syscall1safe n a = syscall1safe_ n (toArg a)

-- | Syscall with 0 parameter
syscall0safe :: Int64 -> IO Int64
syscall0safe = syscall0safe_

