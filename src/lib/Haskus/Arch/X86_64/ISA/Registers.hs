{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | x86 registers
module Haskus.Arch.X86_64.ISA.Registers 
   ( X86Reg
   , RegBank (..)
   , Register
   , regSupportRex
   , regRequireRex
   , getModeRegisters
   , regGPR
   )
where

import Haskus.Arch.Common.Register
import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.RegisterNames

import Data.Set as Set

type Register = X86Reg

---------------------------------------------------------------------
-- ENCODINGS
---------------------------------------------------------------------

-- | Indicate if the register can be encoded with a REX prefix
regSupportRex :: Register -> Bool
regSupportRex r = case r of
   R_AH -> False
   R_BH -> False
   R_CH -> False
   R_DH -> False
   _    -> True

-- | Indicate if the register requires a REX prefix
regRequireRex :: Register -> Bool
regRequireRex r = case r of
   R_BPL -> True
   R_SPL -> True
   R_DIL -> True
   R_SIL -> True
   _     -> registerId r >= 8
   
-- | Create a GPR register from its code
regGPR :: Bool -> Word -> Word -> Register
regGPR useExtRegs sz r
   | r > 15    = error ("Invalid general-purpose register id: " ++ show r)
   -- handle AH,BH,CH,DH
   | not useExtRegs && sz == 8 && 4 <= r && r <= 7 = R_GPRh (r-4)
   | otherwise                                     = R_GPR r sz

   
---------------------------------------------------------------------
-- REGISTERS PER MODE
---------------------------------------------------------------------

-- | Return X86 registers for the selected mode
getModeRegisters :: X86Mode -> Set Register
getModeRegisters mode = Set.unions $ fmap Set.fromList regSets
   where
      regSets = case mode of
         LongMode Long64bitMode ->
            [ regsL64,regsH,regsX64,regsE64,regsR -- General purpose registers (GPRs)
            , regsFPU, regsMMX                    -- 64 bit Media and Floating-Point registers
            , regsXMM 16, regsYMM 16              -- SSE Media registers
            , [R_RIP]                             -- Instruction pointer
            , [R_Flags64]                         -- Flags
            , [R_CS,R_FS,R_GS]                    -- Segments
            ]
         LongMode CompatibilityMode ->
            [ regsL,regsH,regsX,regsE             -- General purpose registers (GPRs)
            , regsFPU, regsMMX                    -- 64 bit Media and Floating-Point registers
            , regsXMM 8, regsYMM 8                -- SSE Media registers
            , [R_EIP]                             -- Instruction pointer
            , [R_Flags32]                         -- Flags
            , regSegs32                           -- Segments
            ]
         LegacyMode ProtectedMode ->
            [ regsL,regsH,regsX,regsE             -- General purpose registers (GPRs)
            , regsFPU, regsMMX                    -- 64 bit Media and Floating-Point registers
            , regsXMM 8, regsYMM 8                -- SSE Media registers
            , [R_EIP]                             -- Instruction pointer
            , [R_Flags32]                         -- Flags
            , regSegs32                           -- Segments
            ]
         LegacyMode Virtual8086Mode ->
            [ regsL,regsH,regsX                   -- General purpose registers (GPRs)
            , regsFPU, regsMMX                    -- 64 bit Media and Floating-Point registers
            , [R_IP]                              -- Instruction pointer
            , [R_Flags16]                         -- Flags
            , regSegs16                           -- Segments
            ]
         LegacyMode RealMode ->
            [ regsL,regsH,regsX                   -- General purpose registers (GPRs)
            , regsFPU                             -- 64 bit Media and Floating-Point registers
            , [R_IP]                              -- Instruction pointer
            , [R_Flags16]                         -- Flags
            , regSegs16                           -- Segments
            ]

      regsL               = fmap (flip R_GPR 8)  [0..3]         -- al,bl,cl,dl
      regsH               = fmap R_GPRh          [0..3]         -- ah,bh,ch,dh
      regsX               = fmap (flip R_GPR 16) [0..7]         -- ax, bx, cx, dx, bp, si, di, sp
      regsE               = fmap (flip R_GPR 32) [0..7]         -- eax, ebx, ecx, edx, ebp, esi, edi, esp
      regsL64             = fmap (flip R_GPR 8)  [0..15]        -- al,...,R15L
      regsX64             = fmap (flip R_GPR 16) [0..15]        -- ax,...,R15W
      regsE64             = fmap (flip R_GPR 32) [0..15]        -- eax,...,R15D
      regsR               = fmap (flip R_GPR 64) [0..15]        -- rax, rbx, ..., r15
      regsFPU             = fmap R_ST            [0..7]         -- ST(0)..ST(7)
      regsMMX             = fmap R_MMX           [0..7]         -- MM0..MM7
      regsXMM       count = fmap R_XMM           [0..count-1]   -- XMM0..XMMn
      regsYMM       count = fmap R_YMM           [0..count-1]   -- YMM0..YMMn
      regSegs16           = [R_CS,R_DS,R_ES,R_SS]
      regSegs32           = [R_CS,R_DS,R_ES,R_FS,R_GS,R_SS]

-- TODO: 32 bit registers are zero-extended into 64 bit registers
-- 16 and 8 bit registers are not zero-extended
