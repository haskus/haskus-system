{-# LANGUAGE CPP, ForeignFunctionInterface #-}


#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE GHCForeignImportPrim, 
             MagicHash, 
             UnboxedTuples,
             UnliftedFFITypes #-}
#endif

module ViperVM.Arch.X86_64.Linux.Syscall
   ( syscall0
   , syscall1
   , syscall2
   , syscall3
   , syscall4
   , syscall5
   , syscall6
   , Arg (..)
) where

import Foreign.Ptr (Ptr, ptrToIntPtr)
import Data.Word (Word64,Word32)

#ifdef __GLASGOW_HASKELL__

--------------------------------------------------
-- Implementation using Haskell foreign primops
--------------------------------------------------

import GHC.Base
import GHC.Int

foreign import prim "x86_64_linux_syscall_primop6" syscall6_# :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop5" syscall5_# :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop4" syscall4_# :: Int# -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop3" syscall3_# :: Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop2" syscall2_# :: Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop1" syscall1_# :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
foreign import prim "x86_64_linux_syscall_primop0" syscall0_# :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)

syscall6_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall6_ (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) (I64# f) = IO $ \s ->
   case (syscall6_# n a b c d e f s) of (# s1, r #) -> (# s1, I64# r #)

syscall5_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall5_ (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) (I64# e) = IO $ \s ->
   case (syscall5_# n a b c d e s) of (# s1, r #) -> (# s1, I64# r #)

syscall4_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall4_ (I64# n) (I64# a) (I64# b) (I64# c) (I64# d) = IO $ \s ->
   case (syscall4_# n a b c d s) of (# s1, r #) -> (# s1, I64# r #)

syscall3_ :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
syscall3_ (I64# n) (I64# a) (I64# b) (I64# c) = IO $ \s ->
   case (syscall3_# n a b c s) of (# s1, r #) -> (# s1, I64# r #)

syscall2_ :: Int64 -> Int64 -> Int64 -> IO Int64
syscall2_ (I64# n) (I64# a) (I64# b) = IO $ \s ->
   case (syscall2_# n a b s) of (# s1, r #) -> (# s1, I64# r #)

syscall1_ :: Int64 -> Int64 -> IO Int64
syscall1_ (I64# n) (I64# a) = IO $ \s ->
   case (syscall1_# n a s) of (# s1, r #) -> (# s1, I64# r #)

syscall0_ :: Int64 -> IO Int64
syscall0_ (I64# n) = IO $ \s ->
   case (syscall0_# n s) of (# s1, r #) -> (# s1, I64# r #)

#else

--------------------------------------------------
-- Implementation using Haskell FFI
--------------------------------------------------

import Data.Int (Int64)
import Data.Word (Word)

foreign import ccall "x86_64_linux_syscall6" syscall6_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall "x86_64_linux_syscall5" syscall5_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall "x86_64_linux_syscall4" syscall4_ :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall "x86_64_linux_syscall3" syscall3_ :: Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall "x86_64_linux_syscall2" syscall2_ :: Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall "x86_64_linux_syscall1" syscall1_ :: Int64 -> Int64 -> IO Int64
foreign import ccall "x86_64_linux_syscall0" syscall0_ :: Int64 -> IO Int64

#endif

class Arg a where
   toArg :: a -> Int64

instance Arg Int where toArg = fromIntegral
instance Arg Int64 where toArg = id
instance Arg Word where toArg = fromIntegral
instance Arg Word64 where toArg = fromIntegral
instance Arg Word32 where toArg = fromIntegral
instance Arg (Ptr a) where toArg = fromIntegral . ptrToIntPtr

syscall6 :: (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Int64 -> a -> b -> c -> d -> e -> f -> IO Int64
syscall6 n a b c d e f = syscall6_ n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e) (toArg f)

syscall5 :: (Arg a, Arg b, Arg c, Arg d, Arg e) => Int64 -> a -> b -> c -> d -> e -> IO Int64
syscall5 n a b c d e = syscall5_ n (toArg a) (toArg b) (toArg c) (toArg d) (toArg e)

syscall4 :: (Arg a, Arg b, Arg c, Arg d) => Int64 -> a -> b -> c -> d -> IO Int64
syscall4 n a b c d = syscall4_ n (toArg a) (toArg b) (toArg c) (toArg d)

syscall3 :: (Arg a, Arg b, Arg c) => Int64 -> a -> b -> c -> IO Int64
syscall3 n a b c = syscall3_ n (toArg a) (toArg b) (toArg c)

syscall2 :: (Arg a, Arg b) => Int64 -> a -> b -> IO Int64
syscall2 n a b = syscall2_ n (toArg a) (toArg b)

syscall1 :: (Arg a) => Int64 -> a -> IO Int64
syscall1 n a = syscall1_ n (toArg a)

syscall0 :: Int64 -> IO Int64
syscall0 = syscall0_
