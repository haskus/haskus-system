{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE InterruptibleFFI #-}

-- | Linux syscall
module Haskus.Arch.X86_64.Linux.Syscall
   ( 
   -- * Primop syscalls
     syscall0primop
   , syscall1primop
   , syscall2primop
   , syscall3primop
   , syscall4primop
   , syscall5primop
   , syscall6primop
   -- * Safe syscalls
   , syscall0safe
   , syscall1safe
   , syscall2safe
   , syscall3safe
   , syscall4safe
   , syscall5safe
   , syscall6safe
   -- * Unsafe syscalls
   , syscall0unsafe
   , syscall1unsafe
   , syscall2unsafe
   , syscall3unsafe
   , syscall4unsafe
   , syscall5unsafe
   , syscall6unsafe
   -- * Interruptible syscalls
   , syscall0interruptible
   , syscall1interruptible
   , syscall2interruptible
   , syscall3interruptible
   , syscall4interruptible
   , syscall5interruptible
   , syscall6interruptible
   )
where

import Haskus.System.Linux.Internals.Arg
import Haskus.Number.Int
import GHC.Base

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
syscall6primop' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall6primop' (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) (I64# f) = IO $ \s ->
   case (syscall6_# n a b c d e f s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 5 parameters
syscall5primop' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall5primop' (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) = IO $ \s ->
   case (syscall5_# n a b c d e s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 4 parameters
syscall4primop' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall4primop' (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) = IO $ \s ->
   case (syscall4_# n a b c d s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 3 parameters
syscall3primop' :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall3primop' (I64# n) (I64# a) (I64# b) (I64# c) = IO $ \s ->
   case (syscall3_# n a b c s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 2 parameters
syscall2primop' :: Int64 -> Int64 -> Int64 -> IO Int64
syscall2primop' (I64# n) (I64# a) (I64# b) = IO $ \s ->
   case (syscall2_# n a b s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 1 parameter
syscall1primop' :: Int64 -> Int64 -> IO Int64
syscall1primop' (I64# n) (I64# a) = IO $ \s ->
   case (syscall1_# n a s) of (# s1, r #) -> (# s1, I64# r #)

-- | Syscall with 0 parameter
syscall0primop :: Int64 -> IO Int64
syscall0primop (I64# n) = IO $ \s ->
   case (syscall0_# n s) of (# s1, r #) -> (# s1, I64# r #)

syscall6primop :: (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Int64 -> a -> b -> c -> d -> e -> f -> IO Int64
syscall6primop n a b c d e f = syscall6primop' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

syscall5primop :: (Arg a, Arg b, Arg c, Arg d, Arg e) => Int64 -> a -> b -> c -> d -> e -> IO Int64
syscall5primop n a b c d e = syscall5primop' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

syscall4primop :: (Arg a, Arg b, Arg c, Arg d) => Int64 -> a -> b -> c -> d -> IO Int64
syscall4primop n a b c d = syscall4primop' n (toArg a) (toArg b) (toArg c) (toArg d)

syscall3primop :: (Arg a, Arg b, Arg c) => Int64 -> a -> b -> c -> IO Int64
syscall3primop n a b c = syscall3primop' n (toArg a) (toArg b) (toArg c)

syscall2primop :: (Arg a, Arg b) => Int64 -> a -> b -> IO Int64
syscall2primop n a b = syscall2primop' n (toArg a) (toArg b)

syscall1primop :: (Arg a) => Int64 -> a -> IO Int64
syscall1primop n a = syscall1primop' n (toArg a)


--------------------------------------------------
-- Implementation using Haskell safe FFI
--------------------------------------------------

foreign import ccall safe "x86_64_linux_syscall6" syscall6safe' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall5" syscall5safe' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall4" syscall4safe' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall3" syscall3safe' :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall2" syscall2safe' :: Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall1" syscall1safe' :: Int64 -> Int64 -> IO Int64
foreign import ccall safe "x86_64_linux_syscall0" syscall0safe :: Int64 -> IO Int64

syscall6safe :: (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Int64 -> a -> b -> c -> d -> e -> f -> IO Int64
syscall6safe n a b c d e f = syscall6safe' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

syscall5safe :: (Arg a, Arg b, Arg c, Arg d, Arg e) => Int64 -> a -> b -> c -> d -> e -> IO Int64
syscall5safe n a b c d e = syscall5safe' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

syscall4safe :: (Arg a, Arg b, Arg c, Arg d) => Int64 -> a -> b -> c -> d -> IO Int64
syscall4safe n a b c d = syscall4safe' n (toArg a) (toArg b) (toArg c) (toArg d)

syscall3safe :: (Arg a, Arg b, Arg c) => Int64 -> a -> b -> c -> IO Int64
syscall3safe n a b c = syscall3safe' n (toArg a) (toArg b) (toArg c)

syscall2safe :: (Arg a, Arg b) => Int64 -> a -> b -> IO Int64
syscall2safe n a b = syscall2safe' n (toArg a) (toArg b)

syscall1safe :: (Arg a) => Int64 -> a -> IO Int64
syscall1safe n a = syscall1safe' n (toArg a)

--------------------------------------------------
-- Implementation using Haskell unsafe FFI
--------------------------------------------------

foreign import ccall unsafe "x86_64_linux_syscall6" syscall6unsafe' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "x86_64_linux_syscall5" syscall5unsafe' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "x86_64_linux_syscall4" syscall4unsafe' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "x86_64_linux_syscall3" syscall3unsafe' :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "x86_64_linux_syscall2" syscall2unsafe' :: Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "x86_64_linux_syscall1" syscall1unsafe' :: Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "x86_64_linux_syscall0" syscall0unsafe :: Int64 -> IO Int64

syscall6unsafe :: (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Int64 -> a -> b -> c -> d -> e -> f -> IO Int64
syscall6unsafe n a b c d e f = syscall6unsafe' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

syscall5unsafe :: (Arg a, Arg b, Arg c, Arg d, Arg e) => Int64 -> a -> b -> c -> d -> e -> IO Int64
syscall5unsafe n a b c d e = syscall5unsafe' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

syscall4unsafe :: (Arg a, Arg b, Arg c, Arg d) => Int64 -> a -> b -> c -> d -> IO Int64
syscall4unsafe n a b c d = syscall4unsafe' n (toArg a) (toArg b) (toArg c) (toArg d)

syscall3unsafe :: (Arg a, Arg b, Arg c) => Int64 -> a -> b -> c -> IO Int64
syscall3unsafe n a b c = syscall3unsafe' n (toArg a) (toArg b) (toArg c)

syscall2unsafe :: (Arg a, Arg b) => Int64 -> a -> b -> IO Int64
syscall2unsafe n a b = syscall2unsafe' n (toArg a) (toArg b)

syscall1unsafe :: (Arg a) => Int64 -> a -> IO Int64
syscall1unsafe n a = syscall1unsafe' n (toArg a)

--------------------------------------------------
-- Implementation using Haskell interruptible FFI
--------------------------------------------------

foreign import ccall interruptible "x86_64_linux_syscall6" syscall6interruptible' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall interruptible "x86_64_linux_syscall5" syscall5interruptible' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall interruptible "x86_64_linux_syscall4" syscall4interruptible' :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall interruptible "x86_64_linux_syscall3" syscall3interruptible' :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall interruptible "x86_64_linux_syscall2" syscall2interruptible' :: Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall interruptible "x86_64_linux_syscall1" syscall1interruptible' :: Int64 -> Int64 -> IO Int64
foreign import ccall interruptible "x86_64_linux_syscall0" syscall0interruptible :: Int64 -> IO Int64

syscall6interruptible :: (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Int64 -> a -> b -> c -> d -> e -> f -> IO Int64
syscall6interruptible n a b c d e f = syscall6interruptible' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

syscall5interruptible :: (Arg a, Arg b, Arg c, Arg d, Arg e) => Int64 -> a -> b -> c -> d -> e -> IO Int64
syscall5interruptible n a b c d e = syscall5interruptible' n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

syscall4interruptible :: (Arg a, Arg b, Arg c, Arg d) => Int64 -> a -> b -> c -> d -> IO Int64
syscall4interruptible n a b c d = syscall4interruptible' n (toArg a) (toArg b) (toArg c) (toArg d)

syscall3interruptible :: (Arg a, Arg b, Arg c) => Int64 -> a -> b -> c -> IO Int64
syscall3interruptible n a b c = syscall3interruptible' n (toArg a) (toArg b) (toArg c)

syscall2interruptible :: (Arg a, Arg b) => Int64 -> a -> b -> IO Int64
syscall2interruptible n a b = syscall2interruptible' n (toArg a) (toArg b)

syscall1interruptible :: (Arg a) => Int64 -> a -> IO Int64
syscall1interruptible n a = syscall1interruptible' n (toArg a)
