{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

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
   , Feature (..)
   , ProcInfo
   , procInfo
   , procVendor
   , procBrand
   , ExtendedFeature (..)
   , procExtFeatures
   )
   where

import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Union
import ViperVM.Format.Binary.Vector as V
import ViperVM.Format.Binary.Bits
import ViperVM.Format.String
import ViperVM.Format.Text as Text
import ViperVM.Utils.Tuple (fromTuple4)
import ViperVM.Utils.Types

#ifdef __GLASGOW_HASKELL__

--------------------------------------------------
-- Implementation using Haskell foreign primops
--------------------------------------------------

import GHC.Base (Word#)
import GHC.Word

foreign import prim "x86_64_cpuid_primop" cpuid# :: Word# -> (# Word#, Word#, Word#, Word# #)

cpuid :: Word32 -> (Word32, Word32, Word32, Word32)
cpuid (W32# n) = 
   case (cpuid# n) of 
      (# a,b,c,d #) -> ( W32# a, W32# b, W32# c, W32# d )

foreign import prim "x86_64_cpuid2_primop" cpuid2# :: Word# -> Word# -> (# Word#, Word#, Word#, Word# #)

cpuid2 :: Word32 -> Word32 -> (Word32, Word32, Word32, Word32)
cpuid2 (W32# eax) (W32# ecx) = 
   case (cpuid2# eax ecx) of 
      (# a,b,c,d #) -> ( W32# a, W32# b, W32# c, W32# d )

#else

--------------------------------------------------
-- Implementation using Haskell FFI
--------------------------------------------------

import Foreign.Marshal.Array
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "x86_64_cpuid_ffi" cpuid_ :: Word32 -> Ptr Word32 -> IO ()

cpuid :: Word32 -> (Word32, Word32, Word32, Word32)
cpuid x = unsafePerformIO $
   allocaArray 4 $ \ptr -> do
      cpuid_ x ptr
      [a,b,c,d] <- peekArray 4 arr
      return (a, b, c, d)

foreign import ccall unsafe "x86_64_cpuid2_ffi" cpuid2_ :: Word32 -> Word32 -> Ptr Word32 -> IO ()

cpuid :: Word32 -> Word32 -> (Word32, Word32, Word32, Word32)
cpuid eax ecx = unsafePerformIO $
   allocaArray 4 $ \ptr -> do
      cpuid_ eax ecx ptr
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

-- | Convert a vector of values into a Text
valToText :: forall (n :: Nat)  (m :: Nat) .
   ( m ~ (n * 4)
   , KnownNat n
   , KnownNat m
   )
   => Vector n Word32 -> Text
valToText xs = Text.pack (fromCStringBuffer v)
   where
      u :: Union '[Vector n Word32 , CStringBuffer m]
      u = toUnion xs

      v :: CStringBuffer m
      v = fromUnion u

procVendor :: Text
procVendor = valToText xs
   where 
      (_,b,c,d) = cpuid 0x00
      xs = V.fromFilledList 0 [b,d,c] :: Vector 3 Word32

procBrand :: Text
procBrand = valToText xs
   where
      -- TODO: add feature check
      x  = fromTuple4 (cpuid 0x80000002)
      y  = fromTuple4 (cpuid 0x80000003)
      z  = fromTuple4 (cpuid 0x80000004)
      xs = V.fromFilledList 0 (x++y++z) :: Vector 12 Word32


type ProcInfo = BitFields Word32
  '[ BitField 6 "padding"         Word8
   , BitField 8 "extended family" Word8
   , BitField 4 "extended model"  Word8
   , BitField 2 "proc type"       Word8
   , BitField 4 "family"          Word8
   , BitField 4 "model"           Word8
   , BitField 4 "stepping"        Word8
   ]

data Feature
   = FeatureFPU
   | FeatureVME
   | FeatureDE
   | FeaturePSE
   | FeatureTSC
   | FeatureMSR
   | FeaturePAE
   | FeatureMCE
   | FeatureCX8
   | FeatureAPIC
   | FeatureReserved0
   | FeatureSEP
   | FeatureMTTRR
   | FeaturePGE
   | FeatureMCA
   | FeatureCMOV
   | FeaturePAT
   | FeaturePSE36
   | FeaturePSN
   | FeatureCLFSH
   | FeatureReserved1
   | FeatureDS
   | FeatureACPI
   | FeatureMMX
   | FeatureFXSR
   | FeatureSSE
   | FeatureSSE2
   | FeatureSS
   | FeatureHTT
   | FeatureTM
   | FeatureIA64
   | FeaturePBE
   | FeatureSSE3
   | FeaturePCLMULQDQ
   | FeatureDTES64
   | FeatureMONITOR
   | FeatureDSCPL
   | FeatureVMX
   | FeatureSMX
   | FeatureEST
   | FeatureTM2
   | FeatureSSSE3
   | FeatureCNXTID
   | FeatureSDBG
   | FeatureFMA
   | FeatureCX16
   | FeatureXTPR
   | FeaturePDCM
   | FeatureReserved2
   | FeaturePCID
   | FeatureDCA
   | FeatureSSE41
   | FeatureSSE42
   | FeatureX2APIC
   | FeatureMOVBE
   | FeaturePOPCNT
   | FeatureTSCDeadline
   | FeatureAES
   | FeatureXSAVE
   | FeatureOSXSAVE
   | FeatureAVX
   | FeatureF16C
   | FeatureRDRND
   | FeatureHypervisor
   deriving (Show,Eq,Enum,CBitSet)

-- | Processor info and feature bits
procInfo :: (ProcInfo, BitSet Word64 Feature)
procInfo = (BitFields a, BitSet.fromBits fs)
   where
      (a,_,c,d) = cpuid 0x01
      fs = (fromIntegral c `shiftL` 32) .|. fromIntegral d


data ExtendedFeature
   = FeatureFsGsBase
   | FeatureTscAdjust
   | FeatureSGX
   | FeatureBMI1
   | FeatureHLE
   | FeatureAVX2
   | FeatureReserved3
   | FeatureSMEP
   | FeatureBMI2
   | FeatureERMS
   | FeatureINVPCID
   | FeatureRTM
   | FeaturePQM
   | FeatureFPUCSDS
   | FeatureMPX
   | FeaturePQE
   | FeatureAVX512F
   | FeatureAVX512DQ
   | FeatureRDSEED
   | FeatureADX
   | FeatureSMAP
   | FeatureAVX512IFMA
   | FeaturePCOMMIT
   | FeatureCLFLUSHOPT
   | FeatureCLWB
   | FeaturePTRACE
   | FeatureAVX512PF
   | FeatureAVX512ER
   | FeatureAVX512CD
   | FeatureSHA
   | FeatureAVX512BW
   | FeatureAVX512VL
   | FeaturePREFETCHWT1
   | FeatureAVX512VBMI
   deriving (Show,Eq,Enum,CBitSet)

-- | Processor info and feature bits
procExtFeatures :: BitSet Word64 ExtendedFeature
procExtFeatures = BitSet.fromBits fs
   where
      (_,b,c,_) = cpuid2 7 0
      fs = ((fromIntegral c .&. 0x03) `shiftL` 32) .|. fromIntegral b
