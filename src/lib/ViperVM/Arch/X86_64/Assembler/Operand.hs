{-# LANGUAGE LambdaCase #-}

module ViperVM.Arch.X86_64.Assembler.Operand
   ( OperandType(..)
   , OperandEnc(..)
   , OperandSpec (..)
   , AccessMode (..)
   , Operand(..)
   , Addr(..)
   , ImmType (..)
   , RegType (..)
   , SubRegType (..)
   , MemType (..)
   , RelType (..)
   , RegFamilies (..)
   , VSIBType (..)
   , VSIBIndexReg (..)
   , maybeOpTypeReg
   )
where

import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.Registers
import ViperVM.Arch.X86_64.Assembler.ModRM

import Data.Word

-- | An operand
data Operand
   = OpImmediate SizedValue            -- ^ Immediate value
   | OpReg Register                    -- ^ Register
   | OpRegPair Register Register       -- ^ REG:REG
   | OpMem MemType Addr                -- ^ Memory address
   | OpCodeAddr Addr                   -- ^ Code address
   | OpPtr16_16 !Word16 !Word16        -- ^ Immediate 16:16 ptr
   | OpPtr16_32 !Word16 !Word32        -- ^ Immediate 16:32 ptr
   | OpRegId !Word8                    -- ^ Register identifier (later to become a OpReg)
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

-- | A memory address
data Addr = Addr
   { addrSeg   :: Register             -- ^ Segment register
   , addrBase  :: Maybe Register       -- ^ Base register
   , addrIndex :: Maybe Register       -- ^ Index register
   , addrScale :: Maybe Scale          -- ^ Scale
   , addrDisp  :: Maybe SizedValue     -- ^ Displacement
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

-- | Immediate type
data ImmType
   = ImmSize8    -- ^ 8-bit immediate
   | ImmSize16   -- ^ 16-bit immediate
   | ImmSizeOp   -- ^ operand-size immediate
   | ImmSizeSE   -- ^ sign-extendable immediate:
                 --     * if sign-extendable bit is set: sign-extended 8-bit immediate
                 --     * if 64-bit operand size: sign-extended 32-bit immediate
                 --     * otherwise: operand-size immediate
   | ImmConst Int -- ^ Constant immediate (used in implicit)
   deriving (Show,Eq)

-- | Memory address type
data MemType
   = MemPair16o32 -- ^ Pair of words in memory (words are operand-size large)
   | Mem8         -- ^ 8-bit memory
   | Mem16        -- ^ 16-bit memory
   | Mem32        -- ^ 32-bit memory
   | Mem64        -- ^ 64-bit memory
   | Mem128       -- ^ 128-bit memory
   | Mem256       -- ^ 256-bit memory
   | Mem512       -- ^ 512-bit memory
   | MemOpSize    -- ^ operand-size-bit memory
   | MemVoid      -- ^ The pointer is used to identify a page, etc. (e.g., CLFLUSH)
   | MemPtr       -- ^ m16:16, m16:32 or m16:64 (16-bit selector + offset)
   | MemDescTable -- ^ Descriptor table: m16&32 (legacy)  or m16&64 (64-bit mode)
   | MemFP        -- ^ m32fp or m64fp (x87)
   | MemFP80      -- ^ m80fp (x87)
   | MemInt       -- ^ m32int or m16int (x87)
   | MemInt64     -- ^ m64int (x87)
   | MemDec80     -- ^ Binary coded decimal (m80dec (x87))
   | MemEnv       -- ^ 14/28 bit FPU environment (x87)
   | MemFPUState  -- ^ 94/108 bit FPU state (x87)
   | MemDSrSI     -- ^ operand-size memory at DS:rSI (rSI depends on address-size, DS if fixed)
   | MemESrDI     -- ^ operand-size memory at ES:rDI (rDI depends on address-size, ES is fixed)
   | MemDSrDI     -- ^ operand-size memory at DS:rDI (rDI depends on address-size, DS is overridable with prefixes)
   | MemVSIB32 VSIBType -- ^ VSIB: 32-bit memory referred to by the VSIB
   | MemVSIB64 VSIBType -- ^ VSIB: 64-bit memory referred to by the VSIB
   | MemState     -- ^ Processor extended states (cf XSAVE/XRSTOR)
   deriving (Show,Eq)

-- | How to use the index register
-- e.g., VSIBType 32 128 --> 32-bit indices in a 128-bits register (XMM)
data VSIBType = VSIBType Size VSIBIndexReg
   deriving (Show,Eq)

-- | Register size for VSIB
data VSIBIndexReg
   = VSIB128
   | VSIB256
   deriving (Show,Eq)

-- | Register type
data RegType
   = RegVec64           -- ^  64-bit vector register (mmx)
   | RegVec128          -- ^ 128-bit vector register (xmm)
   | RegVec256          -- ^ 256-bit vector register (ymm)
   | RegFixed Register  -- ^ Fixed register
   | RegSegment         -- ^ Segment register
   | RegControl         -- ^ Control register
   | RegDebug           -- ^ Debug register
   | Reg8               -- ^ General purpose 8-bit register
   | Reg16              -- ^ General purpose 16-bit register
   | Reg32              -- ^ General purpose 32-bit register
   | Reg64              -- ^ General purpose 64-bit register
   | Reg32o64           -- ^ General purpose 32-bit register in legacy mode,
                        -- general purpose 64-bit register in 64-bit mode
   | RegOpSize          -- ^ General purpose register: 8, 16, 32 or 64-bit
   | RegST              -- ^ x87 register
   | RegCounter         -- ^ CX, ECX or RCX depending on the address-size
   | RegAccu            -- ^ AL, AX, EAX, RAX depending on the operand-size
   | RegStackPtr        -- ^ SP, ESP, RSP (default in 64-bit mode)
   | RegBasePtr         -- ^ BP, EBP, RBP (default in 64-bit mode)
   | RegFam RegFamilies -- ^ Register family
   deriving (Show,Eq)

-- | Register family
data RegFamilies
   = RegFamAX          -- ^ AX, EAX, RAX (depending on operand-size)
   | RegFamBX          -- ^ BX, EBX, RBX (depending on operand-size)
   | RegFamCX          -- ^ CX, ECX, RCX (depending on operand-size)
   | RegFamDX          -- ^ DX, EDX, RDX (depending on operand-size)
   | RegFamSI          -- ^ SI, ESI, RSI (depending on operand-size)
   | RegFamDI          -- ^ DI, EDI, RDI (depending on operand-size)
   | RegFamDXAX        -- ^ AX, DX:AX, EDX:EAX, RDX:RAX
   deriving (Show,Eq)

-- | Sub register type
data SubRegType
   = SubLow8      -- ^ Low  8-bit of a register
   | SubLow16     -- ^ Low 16-bit of a register
   | SubLow32     -- ^ Low 32-bit of a register
   | SubLow64     -- ^ Low 64-bit of a register
   | SubHigh64    -- ^ High 64-bit of a register
   | SubEven64    -- ^ [63:0] and [191:128], etc.
   deriving (Show,Eq)

-- | Relative type
data RelType
   = Rel8         -- ^ Relative 8-bit displacement
   | Rel16o32     -- ^ Relative 16- or 32-bit displacement (16-bit invalid in 64-bit mode)
   deriving (Show,Eq)

-- | Operand types
data OperandType
   = TME OperandType OperandType    -- ^ One of the two types (for ModRM.rm)
   | TLE OperandType OperandType    -- ^ One of the two types depending on Vex.L
   | TWE OperandType OperandType    -- ^ One of the two types depending on Rex.W
   | T_Mem MemType                  -- ^ Memory address
   | T_Reg RegType                  -- ^ Register
   | T_SubReg SubRegType RegType    -- ^ Sub-part of a register
   | T_Pair OperandType OperandType -- ^ Pair (AAA:BBB)
   | T_Imm ImmType                  -- ^ Immediate value
   | T_Rel RelType                  -- ^ Memory offset relative to current IP
   | T_MemOffset                    -- ^ Memory offset relative to the segment base: the offset is address-sized, the value is operand-sized
   | T_MemDSrAX                     -- ^ Memory whose address is DS:EAX or DS:RAX (64-bit mode)
   deriving (Show,Eq)

-- | Operand encoding
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

-- | Operand specification
data OperandSpec = OperandSpec
   { opMode :: AccessMode
   , opType :: OperandType
   , opEnc  :: OperandEnc
   } deriving (Show)

-- | Operand access mode
data AccessMode
   = RO         -- ^ Read-only
   | RW         -- ^ Read-write
   | WO         -- ^ Write-only
   | NA         -- ^ Meta use of the operand
   deriving (Show,Eq)

-- | Indicate if the operand type can be register when stored in ModRM.rm
-- (i.e. ModRM.mod may be 11b)
maybeOpTypeReg :: OperandType -> Bool
maybeOpTypeReg = \case
   TME x y         -> maybeOpTypeReg x || maybeOpTypeReg y
   TLE x y         -> maybeOpTypeReg x || maybeOpTypeReg y
   TWE x y         -> maybeOpTypeReg x || maybeOpTypeReg y
   T_Pair x y      -> maybeOpTypeReg x || maybeOpTypeReg y
   T_Rel _         -> False
   T_Mem _         -> False
   T_Reg _         -> True
   T_SubReg _ _    -> True
   T_Imm _         -> False
   T_MemOffset     -> False
   T_MemDSrAX      -> False
