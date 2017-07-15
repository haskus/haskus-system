{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | x86 registers
module Haskus.Arch.X86_64.ISA.Registers 
   ( X86Reg
   , RegBank (..)
   , Register
   , regSupportRex
   , regRequireRex
   , getRegisterFile
   , regGPR
   )
where

import Haskus.Arch.Common.Register
import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.RegisterNames
import Haskus.Arch.X86_64.ISA.RegisterFile (makeRegSequence,RegisterFile,mergeRegisterFiles,makeRegisterFile)
import qualified Haskus.Arch.X86_64.ISA.RegisterFile as RF

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
-- REGISTER FILE
---------------------------------------------------------------------

-- | Return X86 register file for the selected mode
getRegisterFile :: X86Mode -> RegisterFile
getRegisterFile mode = mergeRegisterFiles . fmap makeRegisterFile $ regSets
   where
      regSets = case mode of
         LongMode Long64bitMode -> [
               concatMap ($ 64) [regsL,regsH,regsX,regsE,regsR], -- General purpose registers (GPRs)
               regsFPU ++ regsMMX,                               -- 64 bit Media and Floating-Point registers
               regsXMM 256 16 ++ regsYMM 256 16,                 -- SSE Media registers
               [regRIP],                                         -- Instruction pointer
               [regRFLAGS],                                      -- Flags
               regSegs64                                         -- Segments
            ]
         LongMode CompatibilityMode -> [
               concatMap ($ 32) [regsL,regsH,regsX,regsE],       -- General purpose registers (GPRs)
               regsFPU ++ regsMMX,                               -- 64 bit Media and Floating-Point registers
               regsXMM 256 8 ++ regsYMM 256 8,                   -- SSE Media registers
               [regEIP],                                         -- Instruction pointer
               [regEFLAGS],                                      -- Flags
               regSegs32                                         -- Segments
            ]
         LegacyMode ProtectedMode -> [
               concatMap ($ 32) [regsL,regsH,regsX,regsE],       -- General purpose registers (GPRs)
               regsFPU ++ regsMMX,                               -- 64 bit Media and Floating-Point registers
               regsXMM 256 8 ++ regsYMM 256 8,                   -- SSE Media registers
               [regEIP],                                         -- Instruction pointer
               [regEFLAGS],                                      -- Flags
               regSegs32                                         -- Segments
            ]
         LegacyMode Virtual8086Mode -> [
               concatMap ($ 16) [regsL,regsH,regsX],             -- General purpose registers (GPRs)
               regsFPU ++ regsMMX,                               -- 64 bit Media and Floating-Point registers
               [regIP],                                          -- Instruction pointer
               [regFLAGS],                                       -- Flags
               regSegs16                                         -- Segments
            ]
         LegacyMode RealMode -> [
               concatMap ($ 16) [regsL,regsH,regsX],             -- General purpose registers (GPRs)
               regsFPU,                                          -- 64 bit Media and Floating-Point registers
               [regIP],                                          -- Instruction pointer
               [regFLAGS],                                       -- Flags
               regSegs16                                         -- Segments
            ]


      regsL pitch         = makeRegSequence 8 pitch 0 ["al","bl","cl","dl"]
      regsH pitch         = makeRegSequence 8 pitch 8 ["ah","bh","ch","dh"]
      regsX pitch         = makeRegSequence 16 pitch 0 ["ax","bx","cx","dx","bp","si","di","sp"]
      regsE pitch         = makeRegSequence 32 pitch 0 ["eax","ebx","ecx","edx","ebp","esi","edi","esp"]
      regsR pitch         = makeRegSequence 64 pitch 0 ["rax","rbx","rcx","rdx","rbp","rsi","rdi","rsp","r8","r9","r10","r11","r12","r13","r14","r15"]
      regsFPU             = makeRegSequence 64 64 0 ["fpr0","fpr1","fpr2","fpr3","fpr4","fpr5","fpr6","fpr7"]
      regsMMX             = makeRegSequence 64 64 0 ["mmx0","mmx1","mmx2","mmx3","mmx4","mmx5","mmx6","mmx7"]
      regsXMM pitch count = makeRegSequence 128 pitch 0 ["xmm" ++ show n | n <- [0..count-1 :: Int]]
      regsYMM pitch count = makeRegSequence 256 pitch 0 ["ymm" ++ show n | n <- [0..count-1 :: Int]]
      regRIP              = RF.Register "rip" 64 0
      regEIP              = RF.Register "eip" 32 0
      regIP               = RF.Register "ip"  16 0
      regRFLAGS           = RF.Register "rflags" 64 0
      regEFLAGS           = RF.Register "eflags" 32 0
      regFLAGS            = RF.Register "flags" 16 0
      regSegs16           = makeRegSequence 16 16 0 ["cs", "ds", "es", "ss"]
      regSegs32           = makeRegSequence 16 16 0 ["cs", "ds", "es", "fs", "gs", "ss"]
      regSegs64           = makeRegSequence 16 16 0 ["cs", "fs", "gs"]

-- TODO: 32 bit registers are zero-extended into 64 bit registers
-- 16 and 8 bit registers are not zero-extended
--
-- TODO: register encoding
-- TODO: only addressable with REX / not addressable with REX prefix (AMD 1 page 27)

