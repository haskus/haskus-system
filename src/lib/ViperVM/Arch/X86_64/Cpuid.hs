{-# LANGUAGE CPP #-}

#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE GHCForeignImportPrim, 
             MagicHash, 
             UnboxedTuples,
             UnliftedFFITypes #-}
#else

{-# ForeignFunctionInterface #-}
#endif

-- | Get processor information with the CPUID instruction
module ViperVM.Arch.X86_64.Cpuid
   ( Cpuid
   , initCpuid
   , cpuid
   , procVendor
   , procBrand
   )
   where

import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS

import ViperVM.Format.Binary.Put

#ifdef __GLASGOW_HASKELL__

--------------------------------------------------
-- Implementation using Haskell foreign primops
--------------------------------------------------

import GHC.Base
import GHC.Word

foreign import prim "x86_64_cpuid_primop" cpuid# :: Word# -> (# Word#, Word#, Word#, Word# #)

cpuid :: Word32 -> (Word32, Word32, Word32, Word32)
cpuid (W32# n) = 
   case (cpuid# n) of 
      (# a,b,c,d #) -> ( W32# a, W32# b, W32# c, W32# d )

#else

--------------------------------------------------
-- Implementation using Haskell FFI
--------------------------------------------------

import Foreign.Marshal.Array
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "x86_64_cpuid_ffi" cpuid_ :: Word32 -> Ptr Word32 -> IO ()

cpuid :: Word32 -> (Word32, Word32, Word32, Word32)
cpuid x = unsafePerformIO $ do
   allocaArray 4 $ \ptr -> do
      cpuid_ x ptr
      [a,b,c,d] <- peekArray 4 arr
      return (a, b, c, d)


#endif

-- | A CPUID context
--
-- The parameter is the maximum allowed value
newtype Cpuid = Cpuid Word32

-- | Initialize a CPUID context
initCpuid :: Cpuid
initCpuid = Cpuid x
   where
      (x,_,_,_) = cpuid 0


procVendor :: Text
procVendor = Text.decodeUtf8 $ runPut $ do
   let (_,b,c,d) = cpuid 0
   putWord32le b
   putWord32le d
   putWord32le c

procBrand :: Text
procBrand = Text.decodeUtf8 $ BS.takeWhile (/= 0) $ runPut $ do
   -- TODO: add feature check
   let f x = do
         let (a,b,c,d) = cpuid x
         putWord32le a
         putWord32le b
         putWord32le c
         putWord32le d
   f 0x80000002
   f 0x80000003
   f 0x80000004
