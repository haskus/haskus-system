-- | X86 architectures support several operating modes.
-- This module gives information for each mode
module Haskus.Arch.X86_64.ISA.Mode
   ( X86Mode (..)
   , LongSubMode (..)
   , LegacySubMode (..)
   , X86Extension(..)
   , allModes
   , allExtensions
   , ModeInfo (..)
   , getModeInfo
   , is64bitMode
   , is32bitMode
   , isLongMode
   , modeName
   -- * Execution mode
   , ExecMode (..)
   , defaultOperationSize
   , defaultAddressSize
   , overriddenOperationSize
   , overriddenAddressSize
   , overriddenOperationSize64
   , defaultStackSize
   , hasExtension
   )
where

import Haskus.Arch.X86_64.ISA.Size


-- | X86 and X86-64 operating mode
data X86Mode
   = LongMode LongSubMode     -- ^ x86-64 long mode
   | LegacyMode LegacySubMode -- ^ x86-32 mode
   deriving (Show,Eq,Ord)

-- | Sub-mode for x86-64
data LongSubMode
   = Long64bitMode
   | CompatibilityMode
   deriving (Show,Eq,Ord)

-- | Sub-mode for x86-32 (legacy)
data LegacySubMode
   = ProtectedMode
   | Virtual8086Mode
   | RealMode
   deriving (Show,Eq,Ord)

-- | All the X86 modes
allModes :: [X86Mode]
allModes  = [ LongMode Long64bitMode
            , LongMode CompatibilityMode
            , LegacyMode RealMode
            , LegacyMode Virtual8086Mode
            , LegacyMode ProtectedMode
            ]

-- | Return the mode name
modeName :: X86Mode -> String
modeName (LongMode Long64bitMode)     = "Long 64-bit mode"
modeName (LongMode CompatibilityMode) = "Long compatibility mode"
modeName (LegacyMode RealMode)        = "Real-mode"
modeName (LegacyMode Virtual8086Mode) = "Virtual 8086 mode"
modeName (LegacyMode ProtectedMode)   = "Protected mode"

data X86Extension
   = VEX             -- ^ VEX encoded instruction support
   | XOP             -- ^ XOP encoded instruction support
   | ADX             -- ^ ADX extension
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
   | MONITOR         -- ^ MONITOR/MWAIT
   | PCLMULQDQ       -- ^ PCLMULQDQ instruction
   | PRFCHW          -- ^ PREFETCHW instruction
   | PREFETCHWT1     -- ^ PREFETCHWT1 instruction
   | FSGSBASE        -- ^ RDFSBASE instruction
   | OSPKE           -- ^ RDPKRU instruction
   | RDRAND          -- ^ RDRAND instruction
   | RDSEDD          -- ^ RDSEED instruction
   | LSAHF           -- ^ LAHF/SAHF instruction in 64-bit mode
   | F16C            -- ^ VCVTPH2PS/VCVTPS2PH instructions
   | FMA             -- ^ Fused multiply-add extension
   | RTM             -- ^ Transactional memory
   | AMD3DNow        -- ^ AMD 3DNow! instructions
   deriving (Show,Eq,Enum,Bounded)

-- | All the X86 extensions
allExtensions :: [X86Extension]
allExtensions = [minBound .. maxBound]

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
is64bitMode _                        = False

-- | Indicate if it is 32 bit mode
is32bitMode :: X86Mode -> Bool
is32bitMode (LongMode CompatibilityMode) = True
is32bitMode (LegacyMode ProtectedMode)   = True
is32bitMode _                            = False

-- | Indicate if it is Long mode
isLongMode :: X86Mode -> Bool
isLongMode (LongMode _) = True
isLongMode _            = False


-- | Execution mode
data ExecMode = ExecMode
   { x86Mode            :: X86Mode        -- ^ x86 mode
   , csDescriptorFlagD  :: Bool           -- ^ D flag: true for 32-bit default sizes
   , ssDescriptorFlagB  :: Bool           -- ^ B flag: true for 32-bit stack size
   , extensions         :: [X86Extension] -- ^ Enabled extensions
   }

-- | Default address size (DAS)
defaultAddressSize :: ExecMode -> AddressSize
defaultAddressSize mode = case x86Mode mode of
   LegacyMode RealMode          -> AddrSize16
   LegacyMode Virtual8086Mode   -> AddrSize16
   LegacyMode ProtectedMode     -> s16o32
   LongMode   CompatibilityMode -> s16o32
   LongMode   Long64bitMode
      | csDescriptorFlagD mode  ->
         error "#GP: D flag in CS descriptor must be 0 in 64-bit mode"
      | otherwise               -> AddrSize64
   where
      s16o32 = if csDescriptorFlagD mode
                  then AddrSize32
                  else AddrSize16

-- | Default operation size (DOS)
defaultOperationSize :: ExecMode -> OperandSize
defaultOperationSize mode = case x86Mode mode of
   LegacyMode RealMode          -> OpSize16
   LegacyMode Virtual8086Mode   -> OpSize16
   LegacyMode ProtectedMode     -> s16o32
   LongMode   CompatibilityMode -> s16o32
   LongMode   Long64bitMode
      | csDescriptorFlagD mode  ->
         error "#GP: D flag in CS descriptor must be 0 in 64-bit mode"
      | otherwise               -> OpSize32
   where
      s16o32 = if csDescriptorFlagD mode
                  then OpSize32
                  else OpSize16

-- | Default stack size (DSS)
defaultStackSize :: ExecMode -> AddressSize
defaultStackSize mode = case x86Mode mode of
   LegacyMode RealMode          -> AddrSize16
   LegacyMode Virtual8086Mode   -> AddrSize16
   LegacyMode ProtectedMode     -> s16o32
   LongMode   CompatibilityMode -> s16o32
   LongMode   Long64bitMode
      | ssDescriptorFlagB mode  ->
         error "#GP: B flag in SS descriptor must be 0 in 64-bit mode"
      | otherwise               -> AddrSize32
   where
      s16o32 = if ssDescriptorFlagB mode
                  then AddrSize32
                  else AddrSize16

-- | Compute the overridden address size (OAS), given the presence or not of the
-- 0x67 prefix
overriddenAddressSize :: Bool -> ExecMode -> AddressSize
overriddenAddressSize False mode = defaultAddressSize mode
overriddenAddressSize True  mode =
   case defaultAddressSize mode of
      AddrSize16 -> AddrSize32
      AddrSize32 -> AddrSize16
      AddrSize64 -> AddrSize32

-- | Compute the overridden operation size (OOS), given the presence or not of
-- the 0x66 prefix
overriddenOperationSize :: Bool -> ExecMode -> OperandSize
overriddenOperationSize False mode = defaultOperationSize mode
overriddenOperationSize True  mode =
   case defaultOperationSize mode of
      OpSize16 -> OpSize32
      _        -> OpSize16

-- | Compute the overridden operation size in 64-bit (OOS64), given the presence
-- or not of the 0x66 prefix, the presence of the W prefix and whether the
-- instruction defaults to 64-bit operation size
overriddenOperationSize64 :: Bool -> Bool -> Bool -> ExecMode -> OperandSize
overriddenOperationSize64 p66 pW p64 mode =
   case x86Mode mode of
      -- in 64-bit mode, most 64-bit instructions default to 32-bit operand
      -- size, except those with the DefaultOperandSize64 property.
      -- REX.W/VEX.W/XOP.W can be used to set a 64-bit operand size (it has
      -- precedence over the 0x66 legacy prefix)
      LongMode Long64bitMode
         | p64 || pW -> OpSize64
      _              -> overriddenOperationSize p66 mode

-- | Indicate if an extension is enabled
hasExtension :: ExecMode -> X86Extension -> Bool
hasExtension mode ext = ext `elem` extensions mode


