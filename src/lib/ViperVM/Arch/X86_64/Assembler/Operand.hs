{-# LANGUAGE LambdaCase #-}

module ViperVM.Arch.X86_64.Assembler.Operand
   ( OperandType(..)
   , OperandEnc(..)
   , OperandSpec (..)
   , AccessMode (..)
   , Operand(..)
   , Addr(..)
   , maybeOpTypeReg
   )
where

import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.ModRM

import Data.Word

-- | An operand
data Operand
   = OpImmediate SizedValue               -- ^ Immediate value
   | OpSignExtendImmediate SizedValue     -- ^ Sign-extended immediate value
   | OpReg Register                       -- ^ Register
   | OpRegPair Register Register          -- ^ REG:REG
   | OpMem Addr                           -- ^ Memory address
   | OpPtr16_16 !Word16 !Word16           -- ^ Immediate 16:16 ptr
   | OpPtr16_32 !Word16 !Word32           -- ^ Immediate 16:32 ptr
   | OpRel SizedValue                     -- ^ Immediate relative
   | OpMask SizedValue                    -- ^ Mask for vector operations

   | OpRegId !Word8                       -- ^ Register identifier (later to become a OpReg)
   deriving (Show,Eq)

-- The X86 architecture supports different kinds of memory addressing. The
-- available addressing modes depend on the execution mode.
-- The most complicated addressing has:
--    - a base register
--    - an index register with a scaling factor (1, 2, 4 or 8)
--    - an offset (displacement)
--
-- Base and index registers can be extended in 64-bit mode to access new registers.
-- Offset size depends on the address size and on the execution mode.

data Addr = Addr
   { addrBase  :: Maybe Register
   , addrIndex :: Maybe Register
   , addrDisp  :: Maybe SizedValue
   , addrScale :: Maybe Scale
   }
   deriving (Show,Eq)

-- Note [Operand size]
-- ~~~~~~~~~~~~~~~~~~~
--
-- Default operand size(s)
-- -----------------------
--   * In virtual 8086-mode, real-mode and system management mode: 16-bit
--   * In protected mode or compatibility mode: 16-bit or 32-bit (a flag is set
--   for each segment)
--   * In 64-bit mode: 32-bit. Some instructions have 64-bit default.
-- 
-- 0x66 prefix
-- -----------
-- In protected mode and compatibility mode, the 0x66 prefix can be used to
-- switch to the second default mode.
--
-- Instruction specific operand size
-- ---------------------------------
-- Some instructions have a bit in the operand indicating whether they use the
-- default operand size or a fixed 8-bit operand size.
--
-- W bit
-- -----
-- REX/VEX/XOP prefixes have a W flag that indicates whether the operand size is
-- the default one or 64-bit. The flag is ignored by some instructions.
--
-- Some instructions only use 32- or 64-bit selected with the W bit (e.g. ADOX).
--
-- L bit
-- -----
-- VEX/XOP prefixes have a L flag that indicates the size of the vector register
-- (XMM or YMM). It can be ignored of fixed at a specified value.
--
-- Immediate operands
-- ------------------
-- Immediate operands can be of the operand size (e.g. MOV)
--
-- More commonly, they are of the operand size *except in 64-bit*:
--    Operand size   | 8 | 16 | 32 | 64
--    Immediate size | 8 | 16 | 32 | 32 (sign-extended)
--
-- Or the immediate size can be fixed to 8-bit and it is sign-extended.
--
-- Or the immediate size can be arbitrarily fixed.
--
-- Per-operand size
-- ----------------
--
-- Some instructions (e.g. CRC32) have one operand that follows REX.W (i.e.
-- 32-bit or 64-bit) while the other one follows the default size (or sizable
-- bit in the opcode).

-- Note [Operands]
-- ~~~~~~~~~~~~~~~
--
-- The ModRM.RM field allows the encoding of either a memory address or a
-- register.
--
-- Only a subset of a register may be used (e.g. the low-order 64-bits of a XMM
-- register).



-- | Operand types
data OperandType
   = TE OperandType OperandType -- ^ One of the two types (for ModRM.rm)

   -- Immediates
   | T_Imm8          -- ^ Word8 immediate
   | T_Imm16         -- ^ Word16 immediate
   | T_Imm8_16_32_64 -- ^ Word16 immediate
   | T_Imm           -- ^ Variable sized immediate
   | T_Rel8          -- ^ Relative 8-bit displacement
   | T_Rel16_32      -- ^ Relative displacement (16-bit invalid in 64-bit mode)
   | T_PTR_16_16     -- ^ Absolute address
   | T_PTR_16_32     -- ^ Absolute address
   | T_PTR16_16_32   -- ^ Absolute address (PTR 16:16 or 16:32)
   | T_Mask          -- ^ Mask for vectors
   | T_3             -- ^ Immediate value 3
   | T_4             -- ^ Immediate value 4

   -- General purpose registers
   | T_R          -- ^ General purpose register: 8 (if sizable bit), 16, 32 or 64-bit (if 64-bit mode supported)
   | T_R16        -- ^ 16-bit general purpose register
   | T_R32        -- ^ 32-bit general purpose register
   | T_R64        -- ^ 64-bit general purpose register
   | T_R32_64     -- ^ 32- or 64-bit general purpose register

   -- Memory
   | T_M_PAIR     -- ^ Pair of words in memory (words are operand-size large)
   | T_M16_XX     -- ^ Pair of words in memory: m16:XX where XX can be 16, 32 or 64
   | T_M64_128    -- ^ 64- or 128-bit memory
   | T_M128       -- ^ 128-bit memory
   | T_M          -- ^ Any memory address
   | T_M16        -- ^ 16-bit memory
   | T_M32        -- ^ 32-bit memory
   | T_M64        -- ^ 64-bit memory
   | T_M16_32     -- ^ 16-bit or 32-bit memory
   | T_M32_64     -- ^ 32-bit or 64-bit memory
   | T_M14_28     -- ^ FPU environement
   | T_M94_108    -- ^ FPU state
   | T_MFP        -- ^ Floating-point value in memory
   | T_M80dec     -- ^ Binary-coded decimal
   | T_M512       -- ^ FXRSTOR, FXSAVE
   | T_M128_256   -- ^ 128- or 256-bit memory
   | T_M16n32_64  -- ^ LGDT/LIDT
   | T_MOffs      -- ^ Moffs8, 16, 32, 64

   -- Vector registers
   | T_Vec           -- ^ Vector register (XMM, YMM, ZMM)
   | T_V64           -- ^ MMX Vector register
   | T_VM64          -- ^ MMX Vector register or 64-bit memory
   | T_V128          -- ^ XMM Vector register
   | T_V256          -- ^ YMM Vector register
   | T_VM128         -- ^ XMM Vector register or memory
   | T_VM256         -- ^ YMM Vector register or memory
   | T_V128_Low32    -- ^ Low 32-bits of a XMM Vector register
   | T_VM128_Low32   -- ^ Low 32-bits of a XMM Vector register or 32-bit memory
   | T_V128_Low64    -- ^ Low 64-bits of a XMM Vector register
   | T_VM128_Low64   -- ^ Low 64-bits of a XMM Vector register or 64-bit memory
   | T_V128_256      -- ^ XMM/YMM Vector register
   | T_VM128_256     -- ^ XMM/YMM Vector register or memory

   -- Specific registers
   | T_Accu       -- ^ Accumulator register (xAX)
   | T_AL_AX_EAX  -- ^ Accumulator registers except RAX
   | T_AX_EAX_RAX -- ^ Accumulator registers except AL
   | T_xDX_xAX    -- ^ The pair (DX:AX), (EDX:EAX) or (RDX:RAX). If 8-bit mode is supported, it is only AX
   | T_xCX_xBX    -- ^ The pair (CX:BX), (ECX:EBX) or (RCX:RBX)
   | T_xAX        -- ^ EAX or RAX
   | T_xBX        -- ^ EBX or RBX
   | T_xCX        -- ^ ECX or RCX
   | T_CX_ECX_RCX -- ^ CX, ECX or RCX
   | T_xDX        -- ^ EDX or RDX
   | T_AL         -- ^ AL register
   | T_AH         -- ^ AH register
   | T_AX         -- ^ AX register
   | T_DX         -- ^ AX register
   | T_XMM0       -- ^ XMM0 register
   | T_rSI        -- ^ DS:rSI
   | T_rDI        -- ^ ES:rDI
   | T_rSP        -- ^ SP, ESP, RSP
   | T_rBP        -- ^ BP, EBP, RBP
   | T_Sreg       -- ^ Segment register
   | T_Creg       -- ^ Control register
   | T_Dreg       -- ^ Debug register

   -- x87
   | T_ST0        -- ^ ST(0)
   | T_ST1        -- ^ ST(1)
   | T_ST         -- ^ ST(i)
   | T_ST_MReal   -- ^ ST(i) register or real memory
   | T_MInt       -- ^ Int memory
   | T_MInt16     -- ^ Int memory
   | T_MInt32     -- ^ Int memory
   | T_MInt64     -- ^ Int memory
   | T_M80bcd     -- ^ 80-bit decimal
   | T_M80real    -- ^ 80-bit real
   deriving (Show)

data OperandEnc
   = RM         -- ^ Operand stored in ModRM.rm
   | Reg        -- ^ Operand stored in ModRM.reg
   | Imm        -- ^ Operand stored in immediate bytes
   | Imm8h      -- ^ Operand stored in bits [7:4] of the immediate byte
   | Imm8l      -- ^ Operand stored in bits [3:0] of the immediate byte
   | Implicit   -- ^ Implicit
   | Vvvv       -- ^ Operand stored in Vex.vvvv field
   | OpcodeLow3 -- ^ Operand stored in opcode 3 last bits
   deriving (Show,Eq)

data OperandSpec = OperandSpec
   { opMode :: AccessMode
   , opType :: OperandType
   , opEnc  :: OperandEnc
   } deriving (Show)

data AccessMode
   = RO         -- ^ Read-only
   | RW         -- ^ Read-write
   | WO         -- ^ Write-only
   deriving (Show,Eq)

-- | Indicate if the operand type can be register when stored in ModRM.rm
-- (i.e. ModRM.mod may be 11b)
maybeOpTypeReg :: OperandType -> Bool
maybeOpTypeReg = \case
   TE x y          -> maybeOpTypeReg x || maybeOpTypeReg y
   T_Imm8          -> False
   T_Imm16         -> False
   T_Imm8_16_32_64 -> False
   T_Imm           -> False
   T_Rel16_32      -> False
   T_Rel8          -> False
   T_PTR_16_16     -> False
   T_PTR_16_32     -> False
   T_PTR16_16_32   -> False
   T_CX_ECX_RCX    -> False
   T_Mask          -> False
   T_3             -> False
   T_4             -> False

   T_R             -> True
   T_R16           -> True
   T_R32           -> True
   T_R64           -> True
   T_R32_64        -> True
   T_Sreg          -> True
   T_Dreg          -> True
   T_Creg          -> True

   T_M_PAIR        -> False
   T_M16_XX        -> False
   T_M16_32        -> False
   T_M64_128       -> False
   T_M32_64        -> False
   T_M64           -> False
   T_M128          -> False
   T_M             -> False
   T_MFP           -> False
   T_M512          -> False
   T_M128_256      -> False
   T_MOffs         -> False

   T_Vec           -> True
   T_V64           -> True
   T_VM64          -> True
   T_V128          -> True
   T_V256          -> True
   T_VM128         -> True
   T_VM256         -> True
   T_V128_Low32    -> True
   T_VM128_Low32   -> True
   T_V128_Low64    -> True
   T_VM128_Low64   -> True
   T_V128_256      -> True
   T_VM128_256     -> True

   T_Accu          -> False
   T_AL_AX_EAX     -> False
   T_AX_EAX_RAX    -> False
   T_xDX_xAX       -> False
   T_xCX_xBX       -> False
   T_xAX           -> False
   T_xBX           -> False
   T_xCX           -> False
   T_xDX           -> False
   T_DX            -> False
   T_AL            -> False
   T_AH            -> False
   T_AX            -> False
   T_XMM0          -> False
   T_rSI           -> False
   T_rDI           -> False
   T_rSP           -> False
   T_rBP           -> False

   T_ST0           -> False
   T_ST1           -> False
   T_ST            -> True
   T_ST_MReal      -> True
   T_MInt          -> False
   T_MInt16        -> False
   T_MInt32        -> False
   T_MInt64        -> False
   T_M80real       -> False
   T_M80dec        -> False
   T_M80bcd        -> False
   T_M16           -> False
   T_M32           -> False
   T_M14_28        -> False
   T_M94_108       -> False
   T_M16n32_64     -> False

