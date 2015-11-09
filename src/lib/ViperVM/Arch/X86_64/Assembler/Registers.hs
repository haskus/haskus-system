-- | x86 registers
module ViperVM.Arch.X86_64.Assembler.Registers 
   ( Register(..)
   , RegFamily(..)
   , getRegisterFile
   , regFromCode
   )
where

import Data.Word
import Data.Bits
import Data.Maybe (fromJust)

import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.RegisterFile (makeRegSequence,RegisterFile,mergeRegisterFiles,makeRegisterFile)
import qualified ViperVM.Arch.X86_64.Assembler.RegisterFile as RF

data RegFamily
   = RF_GPR    -- general purpose register: xL, xX, ExX, RxX
   | RF_X87 
   | RF_MMX
   | RF_XMM
   | RF_YMM
   | RF_ZMM
   | RF_VEC    -- vector register: MMX (64), XMM (128), YMM (256), ZMM (512)
   | RF_SEG
   | RF_CTRL
   | RF_DBG
   deriving (Show,Eq)

data Register
   = R_AL | R_BL | R_CL | R_DL
   | R_AH | R_BH | R_CH | R_DH
   | R_AX | R_BX | R_CX | R_DX
   | R_BP | R_SP | R_DI | R_SI
   | R_BPL | R_SPL | R_DIL | R_SIL
   | R_EAX | R_EBX | R_ECX | R_EDX
   | R_EBP | R_ESP | R_EDI | R_ESI
   | R_RAX | R_RBX | R_RCX | R_RDX
   | R_RBP | R_RSP | R_RDI | R_RSI
   | R_R8  | R_R9  | R_R10 | R_R11
   | R_R12 | R_R13 | R_R14 | R_R15
   | R_R8B | R_R9B | R_R10B| R_R11B
   | R_R12B| R_R13B| R_R14B| R_R15B
   | R_R8W | R_R9W | R_R10W| R_R11W
   | R_R12W| R_R13W| R_R14W| R_R15W
   | R_R8D | R_R9D | R_R10D| R_R11D
   | R_R12D| R_R13D| R_R14D| R_R15D
   | R_ST Word8
   | R_CR Word8
   | R_DR Word8
   | R_MMX Word8
   | R_XMM Word8
   | R_YMM Word8
   | R_ZMM Word8
   | R_CS  | R_DS  | R_ES  | R_FS
   | R_GS  | R_SS
   | R_IP  | R_EIP | R_RIP
   deriving (Show,Eq)

regFromCode :: RegFamily -> Maybe Size -> Bool -> Word8 -> Register
regFromCode fm sz useExtRegs code = case fm of
   RF_GPR -> case (fromJust sz, code, useExtRegs) of
      (Size8, 0, _)     -> R_AL
      (Size8, 1, _)     -> R_CL
      (Size8, 2, _)     -> R_DL
      (Size8, 3, _)     -> R_BL
      (Size8, 4, True)  -> R_SPL
      (Size8, 4, False) -> R_AH
      (Size8, 5, True)  -> R_BPL
      (Size8, 5, False) -> R_CH
      (Size8, 6, True)  -> R_SIL
      (Size8, 6, False) -> R_DH
      (Size8, 7, True)  -> R_DIL
      (Size8, 7, False) -> R_BH
      (Size8, 8, True)  -> R_R8B
      (Size8, 9, True)  -> R_R9B
      (Size8,10, True)  -> R_R10B
      (Size8,11, True)  -> R_R11B
      (Size8,12, True)  -> R_R12B
      (Size8,13, True)  -> R_R13B
      (Size8,14, True)  -> R_R14B
      (Size8,15, True)  -> R_R15B

      (Size16, 0, _)    -> R_AX
      (Size16, 1, _)    -> R_CX
      (Size16, 2, _)    -> R_DX
      (Size16, 3, _)    -> R_BX
      (Size16, 4, _)    -> R_SP
      (Size16, 5, _)    -> R_BP
      (Size16, 6, _)    -> R_SI
      (Size16, 7, _)    -> R_DI
      (Size16, 8, True) -> R_R8W
      (Size16, 9, True) -> R_R9W
      (Size16,10, True) -> R_R10W
      (Size16,11, True) -> R_R11W
      (Size16,12, True) -> R_R12W
      (Size16,13, True) -> R_R13W
      (Size16,14, True) -> R_R14W
      (Size16,15, True) -> R_R15W

      (Size32, 0, _)    -> R_EAX
      (Size32, 1, _)    -> R_ECX
      (Size32, 2, _)    -> R_EDX
      (Size32, 3, _)    -> R_EBX
      (Size32, 4, _)    -> R_ESP
      (Size32, 5, _)    -> R_EBP
      (Size32, 6, _)    -> R_ESI
      (Size32, 7, _)    -> R_EDI
      (Size32, 8, True) -> R_R8D
      (Size32, 9, True) -> R_R9D
      (Size32,10, True) -> R_R10D
      (Size32,11, True) -> R_R11D
      (Size32,12, True) -> R_R12D
      (Size32,13, True) -> R_R13D
      (Size32,14, True) -> R_R14D
      (Size32,15, True) -> R_R15D

      (Size64, 0, _)    -> R_RAX
      (Size64, 1, _)    -> R_RCX
      (Size64, 2, _)    -> R_RDX
      (Size64, 3, _)    -> R_RBX
      (Size64, 4, _)    -> R_RSP
      (Size64, 5, _)    -> R_RBP
      (Size64, 6, _)    -> R_RSI
      (Size64, 7, _)    -> R_RDI
      (Size64, 8, True) -> R_R8
      (Size64, 9, True) -> R_R9
      (Size64,10, True) -> R_R10
      (Size64,11, True) -> R_R11
      (Size64,12, True) -> R_R12
      (Size64,13, True) -> R_R13
      (Size64,14, True) -> R_R14
      (Size64,15, True) -> R_R15
      _ -> error $ "Invalid GPR register index: " ++ show code


   RF_X87 -> if code <= 7
      then R_ST code
      else error $ "Invalid X87 register index: " ++ show code

   RF_MMX -> if code <= (if useExtRegs then 15 else 7)
      then R_MMX (code .&. 0x07)
      else error $ "Invalid MMX register index: " ++ show code

   RF_XMM -> if code <= (if useExtRegs then 15 else 7)
      then R_XMM code
      else error $ "Invalid XMM register index: " ++ show code

   RF_YMM -> if code <= (if useExtRegs then 15 else 7)
      then R_YMM code
      else error $ "Invalid YMM register index: " ++ show code

   RF_ZMM -> if code <= (if useExtRegs then 15 else 7)
      then R_ZMM code
      else error $ "Invalid ZMM register index: " ++ show code

   RF_SEG -> case code .&. 0x07 of
      0  -> R_ES
      1  -> R_CS
      2  -> R_SS
      3  -> R_DS
      4  -> R_FS
      5  -> R_GS
      _ -> error $ "Invalid segment register index: " ++ show code

   RF_CTRL -> if code <= (if useExtRegs then 15 else 7)
      then R_CR code
      else error $ "Invalid CR register index: " ++ show code

   RF_DBG -> if code <= (if useExtRegs then 15 else 7)
      then R_DR code
      else error $ "Invalid DR register index: " ++ show code

   RF_VEC -> case sz of
      Just Size64  -> regFromCode RF_MMX Nothing useExtRegs code
      Just Size128 -> regFromCode RF_XMM Nothing useExtRegs code
      Just Size256 -> regFromCode RF_YMM Nothing useExtRegs code
      Just Size512 -> regFromCode RF_ZMM Nothing useExtRegs code
      Just s       -> error $ "Invalid vector size: " ++ show s
      Nothing      -> error "Vector register without size"


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


      regsL pitch = makeRegSequence 8 pitch 0 ["al","bl","cl","dl"]
      regsH pitch = makeRegSequence 8 pitch 8 ["ah","bh","ch","dh"]
      regsX pitch = makeRegSequence 16 pitch 0 ["ax","bx","cx","dx","bp","si","di","sp"]
      regsE pitch = makeRegSequence 32 pitch 0 ["eax","ebx","ecx","edx","ebp","esi","edi","esp"]
      regsR pitch = makeRegSequence 64 pitch 0 ["rax","rbx","rcx","rdx","rbp","rsi","rdi","rsp","r8","r9","r10","r11","r12","r13","r14","r15"]
      regsFPU = makeRegSequence 64 64 0 ["fpr0","fpr1","fpr2","fpr3","fpr4","fpr5","fpr6","fpr7"]
      regsMMX = makeRegSequence 64 64 0 ["mmx0","mmx1","mmx2","mmx3","mmx4","mmx5","mmx6","mmx7"]
      regsXMM pitch count = makeRegSequence 128 pitch 0 ["xmm" ++ show n | n <- [0..count-1 :: Int]]
      regsYMM pitch count = makeRegSequence 256 pitch 0 ["ymm" ++ show n | n <- [0..count-1 :: Int]]
      regRIP = RF.Register "rip" 64 0
      regEIP = RF.Register "eip" 32 0
      regIP  = RF.Register "ip"  16 0
      regRFLAGS = RF.Register "rflags" 64 0
      regEFLAGS = RF.Register "eflags" 32 0
      regFLAGS = RF.Register "flags" 16 0
      regSegs16 = makeRegSequence 16 16 0 ["cs", "ds", "es", "ss"]
      regSegs32 = makeRegSequence 16 16 0 ["cs", "ds", "es", "fs", "gs", "ss"]
      regSegs64 = makeRegSequence 16 16 0 ["cs", "fs", "gs"]

-- TODO: 32 bit registers are zero-extended into 64 bit registers
-- 16 and 8 bit registers are not zero-extended
--
-- TODO: register encoding
-- TODO: only addressable with REX / not addressable with REX prefix (AMD 1 page 27)
