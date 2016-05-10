-- | X86 architectures support several operating modes.
-- This module gives information for each mode
module ViperVM.Arch.X86_64.Assembler.Mode 
   ( X86Mode (..)
   , LongSubMode (..)
   , LegacySubMode (..)
   , Flag (..)
   , X86Extension(..)
   , ModeInfo (..)
   , getModeInfo
   , is64bitMode
   , is32bitMode
   , isLongMode
   ) where


-- | X86 and X86-64 operating mode
data X86Mode
   = LongMode LongSubMode     -- ^ x86-64 long mode
   | LegacyMode LegacySubMode -- ^ x86-32 mode
   deriving (Show,Eq)

-- | Sub-mode for x86-64
data LongSubMode
   = Long64bitMode 
   | CompatibilityMode 
   deriving (Show,Eq)

-- | Sub-mode for x86-32 (legacy)
data LegacySubMode
   = ProtectedMode 
   | Virtual8086Mode 
   | RealMode 
   deriving (Show,Eq)

-- | Status flag
data Flag
   -- Status flag
   = CF     -- ^ Carry flag
   | PF     -- ^ Parity flag
   | AF     -- ^ Adjust flag
   | ZF     -- ^ Zero flag
   | SF     -- ^ Sign flag
   | TF     -- ^ Trap flag
   | OF     -- ^ Overflow flag

   -- Control flags
   | DF     -- ^ Direction flag
   | IF     -- ^ Interrupt flag
   | AC     -- ^ Alignment check
   deriving (Show,Enum,Eq)

data X86Extension
   = ADX             -- ^ ADX extension
   | MMX             -- ^ MMX
   | AVX             -- ^ AVX extension
   | AVX2            -- ^ AVX2 extension
   | SSE             -- ^ SSE extension
   | SSE2            -- ^ SSE2 extension
   | SSE3            -- ^ SSE3 extension
   | SSSE3           -- ^ SSSE3 extension
   | SSE4_1          -- ^ SSE4.1 extension
   | SSE4_2          -- ^ SSE4.2 extension
   | AES             -- ^ AES extension
   | BMI1            -- ^ BMI1 extension
   | BMI2            -- ^ BMI2 extension
   | SMAP            -- ^ Supervisor Mode Access Prevention (SMAP)
   | CLFLUSH         -- ^ CLFLUSH instruction
   | CX8             -- ^ CMPXCHG8B instruction
   | FPU             -- ^ x87 instructions
   | CMOV            -- ^ CMOVs instructions (and FCMOVcc if FPU is set too)
   | INVPCID         -- ^ Invalid process-context identifier (INVPCID) extension
   | LAHF            -- ^ LAHF extension in 64-bit mode
   | MONITOR         -- ^ MONITOR/MWAIT
   | PCLMULQDQ       -- ^ PCLMULQDQ instruction
   deriving (Show,Eq)


-- | IP-relative addressing support
data RelativeAddressing
   = FullRelativeAddressing      -- ^ Supported by all instructions
   | ControlRelativeAddressing   -- ^ Supported by control instructions
   deriving (Show,Eq)

-- | Information on a given mode
data ModeInfo = ModeInfo
   { relativeAddressing :: RelativeAddressing -- ^ IP-relative addressing support
   }

-- | Return information for the selected mode
getModeInfo :: X86Mode -> ModeInfo
getModeInfo mode = case mode of

   LongMode Long64bitMode     -> ModeInfo {
      relativeAddressing         = FullRelativeAddressing
   }

   LongMode CompatibilityMode -> ModeInfo {
      relativeAddressing         = ControlRelativeAddressing
   }

   LegacyMode ProtectedMode   -> ModeInfo {
      relativeAddressing         = ControlRelativeAddressing
   }

   LegacyMode Virtual8086Mode -> ModeInfo {
      relativeAddressing         = ControlRelativeAddressing
   }

   LegacyMode RealMode        -> ModeInfo {
      relativeAddressing         = ControlRelativeAddressing
   }

-- | Indicate if it is 64 bit mode
is64bitMode :: X86Mode -> Bool
is64bitMode (LongMode Long64bitMode) = True
is64bitMode _ = False

-- | Indicate if it is 32 bit mode
is32bitMode :: X86Mode -> Bool
is32bitMode (LongMode CompatibilityMode) = True
is32bitMode (LegacyMode ProtectedMode) = True
is32bitMode _ = False

-- | Indicate if it is Long mode
isLongMode :: X86Mode -> Bool
isLongMode (LongMode _) = True
isLongMode _ = False
