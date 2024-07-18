{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Haskus.Arch.X86_64.ISA.Assembler where

import Data.Word
import Data.Int
import Numeric


data Opcode
  = AAA
  | ADC
  deriving (Show,Eq,Ord)

data Reg
  = AL | AX | EAX | RAX | AH
  | BL | BX | EBX | RBX | BH
  | CL | CX | ECX | RCX | CH
  | DL | DX | EDX | RDX | DH
  | BP | EBP | RBP
  | SP | ESP | RSP
  deriving (Show,Eq,Ord)

-- | TODO: sublanguage for memory addresses with labels/expressions
data Expr
  = ELabel String

data Mem
  = MemAddr !Word64
  | MemExpr !Expr
  | MemSIB  !Reg !Int8 !Reg

data Imm
  = Imm8   !Word8
  | Imm16  !Word16
  | Imm32  !Word32
  | Imm64  !Word64
  | Imm8s  !Int8
  | Imm16s !Int16
  | Imm32s !Int32
  | Imm64s !Int64
  | ImmX   !Integer

data Operand
  = OpReg !Reg
  | OpMem !Mem
  | OpImm !Imm

data InsnOpts
  = Lock

data InsnFlags
  = Arch8086
  | NoLong
  | Long
  | SM -- ?
  | CanLock


data Insn = Insn InsnOpts Opcode [Operand]

newtype Asm a
  = Asm (IO a)
  deriving newtype (Functor,Applicative,Monad)

emit :: Word8 -> Asm ()
emit x = Asm $ putStr (showHex x "")

-- | Check that the instruction flags are valid for the current context.
check :: InsnOpts -> [InsnFlags] -> Asm ()
check opts flags = do
  if | Lock `elem` opts && CanLock `notElem` flags -> error "Can't lock this"
  return ()

assembleList :: [Insn] -> Asm ()
assembleList is = mapM_ assembleInsn is

assembleInsn :: Insn -> Asm ()
assembleInsn (Insn opts oc ops) = assemble opts oc ops

assemble :: InsnOpts -> Opcode -> [Operand] -> Asm ()
assemble opts = \cases
  AAA [] -> do
    check opts [Arch8086,NoLong]
    emit 0x37
  
  AAD [] -> do
    check opts [Arch8086,NoLong]
    emit 0xd5
    emit 0x0a
  
  AAD [x] -> do
    check opts [Arch8086,NoLong]
    i <- expectImm8u x
