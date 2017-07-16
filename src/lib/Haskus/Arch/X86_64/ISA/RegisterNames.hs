{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

-- | Register names as used in x86 documentation
module Haskus.Arch.X86_64.ISA.RegisterNames
   ( RegBank (..)
   , X86Reg
   , registerName
   -- * Special registers
   , pattern R_CR32
   , pattern R_CR64
   , pattern R_DR32
   , pattern R_DR64
   -- * Flags registers
   , pattern R_Flags
   , pattern R_Flags16
   , pattern R_Flags32
   , pattern R_Flags64
   -- * Vector registers
   , pattern R_MMX
   , pattern R_XMM
   , pattern R_YMM
   , pattern R_ZMM
   -- * FPU registers
   , pattern R_ST
   -- * Segment registers
   , pattern R_Seg
   , pattern R_ES
   , pattern R_CS
   , pattern R_FS
   , pattern R_DS
   , pattern R_SS
   , pattern R_GS
   -- * Instruction pointers
   , pattern R_IP
   , pattern R_EIP
   , pattern R_RIP
   -- * General purpose registers
   , pattern R_GPRh
   , pattern R_GPR
   , pattern R_AL 
   , pattern R_BL 
   , pattern R_CL 
   , pattern R_DL
   , pattern R_AH 
   , pattern R_BH 
   , pattern R_CH 
   , pattern R_DH
   , pattern R_AX 
   , pattern R_BX 
   , pattern R_CX 
   , pattern R_DX
   , pattern R_BP 
   , pattern R_SP 
   , pattern R_DI 
   , pattern R_SI
   , pattern R_BPL 
   , pattern R_SPL 
   , pattern R_DIL 
   , pattern R_SIL
   , pattern R_EAX 
   , pattern R_EBX 
   , pattern R_ECX 
   , pattern R_EDX
   , pattern R_EBP 
   , pattern R_ESP 
   , pattern R_EDI 
   , pattern R_ESI
   , pattern R_RAX 
   , pattern R_RBX 
   , pattern R_RCX 
   , pattern R_RDX
   , pattern R_RBP 
   , pattern R_RSP 
   , pattern R_RDI 
   , pattern R_RSI
   , pattern R_R8  
   , pattern R_R9  
   , pattern R_R10 
   , pattern R_R11
   , pattern R_R12 
   , pattern R_R13 
   , pattern R_R14 
   , pattern R_R15
   , pattern R_R8L 
   , pattern R_R9L 
   , pattern R_R10L
   , pattern R_R11L
   , pattern R_R12L
   , pattern R_R13L
   , pattern R_R14L
   , pattern R_R15L
   , pattern R_R8W 
   , pattern R_R9W 
   , pattern R_R10W
   , pattern R_R11W
   , pattern R_R12W
   , pattern R_R13W
   , pattern R_R14W
   , pattern R_R15W
   , pattern R_R8D 
   , pattern R_R9D 
   , pattern R_R10D
   , pattern R_R11D
   , pattern R_R12D
   , pattern R_R13D
   , pattern R_R14D
   , pattern R_R15D
   )
where

import Haskus.Arch.Common.Register

-- Note [Registers]
-- ~~~~~~~~~~~~~~~~~~~~~
--
-- Names history
-- -------------
--
-- General purpose registers:
--
--    * In the 8080, 8-bit registers were single letters: A,B,C,D...
--
--    * In the 8086, registers have been extended to 16-bit, hence their names
--    were extended too: AX,BX,CX,DX...  It was also possible to index
--    sub-registers (lower and higher parts), hence the aliased registers:
--    AH,AL,BH,BL,CH,CL...
--
--    * In the 80386, registers have been extended again to 32-bit. We could
--    have had AXX, BXX, etc. but instead we've got: EAX,EBX,ECX...
--
--    * With x86-64, registers have been extended again: RAX,RBX,RCX...
--    Additional general purpose registers have been added. We could have had
--    REX,RFX,RGX... but instead we've got: R8,R9,R10... and their aliased
--    sub-registers are R8L (low), R8W (word), R8D (double-word), hence R8 is in
--    fact R8Q (quad-word) or REX.
--
-- Vector registers:
--
--    * It started with MMX registers: MM0-MM7 ("MultiMedia eXtension", 64-bit).
--    These registers alias the FPU stack (st(0),st(1),etc. 80-bit: the
--    extraneous bits of the FPU registers are set to 1 when MMX is used and
--    their tag word is set to "valid").
--
--    * SSE (Streaming SIMD Extensions) introduced XMM registers: XMM0-XMM7
--    (128-bit). They don't alias the MMX registers!
--
--    * AVX (Advanced Vector Extensions) introduced YMM registers: YMM0-YMM15
--    (256-bit), aliasing XMM registers. XMM8-XMM15 have been added too.
--
--    * AVX-512 introduced ZMM registers: ZMM0-ZMM31 (512-bit), aliasing YMM
--    registers. YMM and XMM registers have been added accordingly.
--
--    We're eager to know the naming of the future generation! XZMM? ZMMX? ZZMM?
--
-- Register naming
-- ---------------
--
-- To clean up the naming mess, we use the following representation:
--
--    Reg b n s o
--       where b = Bank
--             n = register id
--             s = size
--             o = offset
--
--    E.g.,
--       G is for general purpose register
--       V is for vector registers
--
--    R G 0 32 0  = EAX
--    R G 1 64 0  = RCX (yes B,C and D are shuffled in X86's encoding)
--    R G 2 16 0  = DX
--    R G 3 8  0  = BL
--    R G 3 8  8  = BH
--    R V 0 128 0 = XMM0
--    R V 1 256 0 = YMM1
--
-- So hopefully we can forget about the crazy inherited naming.
--
--
-- Sub-registers
-- -------------
--
-- When using a sub-register, it can have an effect on the whole register or
-- on the whole register bank:
--    * AVX-128 operation on an XMM register may zero the upper part (in YMM,ZMM)
--    * MMX operations modify the ST registers and their tags
--
-- Some sub-registers are not encodable in every context (legacy AH,BH,etc.,
-- newer DIL,SIL,etc.).
--
-- SSE operations on XMM registers that have been used by an AVX
-- operations may be more costly:
--    * the system has to maintain the contents of the upper parts of YMM,ZMM if
--    they aren't set to zero
--    * VZEROUPPER and VZEROALL AVX instructions can be used to switch back to
--    SSE without additional overhead
--
-- See:
-- * https://software.intel.com/en-us/articles/intel-avx-state-transitions-migrating-sse-code-to-avx
-- * https://software.intel.com/en-us/articles/avoiding-avx-sse-transition-penalties
--

-- | X86 register banks
data RegBank
   = GPR     -- ^ General purpose register bank
   | Vec     -- ^ Vector register bank
   | FP      -- ^ FP stack bank
   | Seg     -- ^ Segment bank
   | Control -- ^ Control register bank
   | Debug   -- ^ Debug register bank
   | IP      -- ^ Instruction pointer
   | Flags   -- ^ Flags
   deriving (Show,Eq,Ord)

type X86Reg = Reg RegBank

-- | Match FPU registers
pattern R_ST :: Word -> X86Reg
pattern R_ST t = Reg FP t 80 0

-- | Match MMX registers
pattern R_MMX :: Word -> X86Reg
pattern R_MMX t = Reg FP t 64 0

-- | Match XMM registers
pattern R_XMM :: Word -> X86Reg
pattern R_XMM t = Reg Vec t 128 0

-- | Match YMM registers
pattern R_YMM :: Word -> X86Reg
pattern R_YMM t = Reg Vec t 256 0

-- | Match ZMM registers
pattern R_ZMM :: Word -> X86Reg
pattern R_ZMM t = Reg Vec t 512 0

-- | Match control register in 32-bit modes
pattern R_CR32 :: Word -> X86Reg
pattern R_CR32 t = Reg Control t 32 0

-- | Match control register in 64-bit modes
pattern R_CR64 :: Word -> X86Reg
pattern R_CR64 t = Reg Control t 64 0

-- | Debug registers
pattern R_DR32 :: Word -> X86Reg
pattern R_DR32 t = Reg Debug t 32 0

-- | Debug registers
pattern R_DR64 :: Word -> X86Reg
pattern R_DR64 t = Reg Debug t 64 0

---------------------------------------
-- Flags registers
---------------------------------------

-- | Flags registers
pattern R_Flags :: Word -> X86Reg
pattern R_Flags s = Reg Flags 0 s 0

-- | Flags16 register
pattern R_Flags16 :: X86Reg
pattern R_Flags16 = Reg Flags 0 16 0

-- | Flags32 register
pattern R_Flags32 :: X86Reg
pattern R_Flags32 = Reg Flags 0 32 0

-- | Flags64 register
pattern R_Flags64 :: X86Reg
pattern R_Flags64 = Reg Flags 0 64 0

---------------------------------------
-- Segment registers
---------------------------------------

-- | Segment registers
pattern R_Seg :: Word -> X86Reg
pattern R_Seg t = Reg Seg t 16 0

-- | ES segment
pattern R_ES :: X86Reg
pattern R_ES = R_Seg 0

-- | CS segment
pattern R_CS :: X86Reg
pattern R_CS = R_Seg 1

-- | SS segment
pattern R_SS :: X86Reg
pattern R_SS = R_Seg 2

-- | DS segment
pattern R_DS :: X86Reg
pattern R_DS = R_Seg 3

-- | FS segment
pattern R_FS :: X86Reg
pattern R_FS = R_Seg 4

-- | GS segment
pattern R_GS :: X86Reg
pattern R_GS = R_Seg 5


---------------------------------------
-- Instruction pointer
---------------------------------------

-- | IP
pattern R_IP :: X86Reg
pattern R_IP = Reg IP 0 16 0

-- | EIP
pattern R_EIP :: X86Reg
pattern R_EIP = Reg IP 0 32 0

-- | RIP
pattern R_RIP :: X86Reg
pattern R_RIP = Reg IP 0 64 0

---------------------------------------
-- General purpose registers
---------------------------------------

-- | Match general purpose higher 8-bit registers
pattern R_GPRh :: Word -> X86Reg
pattern R_GPRh t = Reg GPR t 8 8

-- | General purpose register
pattern R_GPR :: Word -> Word -> X86Reg
pattern R_GPR t sz = Reg GPR t sz 0

------------
-- 8 bit
------------

-- | AL
pattern R_AL :: X86Reg
pattern R_AL = R_GPR 0 8

-- | CL
pattern R_CL :: X86Reg
pattern R_CL = R_GPR 1 8

-- | DL
pattern R_DL :: X86Reg
pattern R_DL = R_GPR 2 8

-- | BL
pattern R_BL :: X86Reg
pattern R_BL = R_GPR 3 8

-- | AH
pattern R_AH :: X86Reg
pattern R_AH = R_GPRh 0

-- | CH
pattern R_CH :: X86Reg
pattern R_CH = R_GPRh 1

-- | DH
pattern R_DH :: X86Reg
pattern R_DH = R_GPRh 2

-- | BH
pattern R_BH :: X86Reg
pattern R_BH = R_GPRh 3


-- | SPL
pattern R_SPL :: X86Reg
pattern R_SPL = R_GPR 4 8

-- | BPL
pattern R_BPL :: X86Reg
pattern R_BPL = R_GPR 5 8

-- | SIL
pattern R_SIL :: X86Reg
pattern R_SIL = R_GPR 6 8

-- | DIL
pattern R_DIL :: X86Reg
pattern R_DIL = R_GPR 7 8

-- | R8L
pattern R_R8L :: X86Reg
pattern R_R8L = R_GPR 8 8

-- | R9L
pattern R_R9L :: X86Reg
pattern R_R9L = R_GPR 9 8

-- | R10L
pattern R_R10L :: X86Reg
pattern R_R10L = R_GPR 10 8

-- | R11L
pattern R_R11L :: X86Reg
pattern R_R11L = R_GPR 11 8

-- | R12L
pattern R_R12L :: X86Reg
pattern R_R12L = R_GPR 12 8

-- | R13L
pattern R_R13L :: X86Reg
pattern R_R13L = R_GPR 13 8

-- | R14L
pattern R_R14L :: X86Reg
pattern R_R14L = R_GPR 14 8

-- | R15L
pattern R_R15L :: X86Reg
pattern R_R15L = R_GPR 15 8

------------
-- 16 bit
------------

-- | AX
pattern R_AX :: X86Reg
pattern R_AX = R_GPR 0 16

-- | CX
pattern R_CX :: X86Reg
pattern R_CX = R_GPR 1 16

-- | DX
pattern R_DX :: X86Reg
pattern R_DX = R_GPR 2 16

-- | BX
pattern R_BX :: X86Reg
pattern R_BX = R_GPR 3 16

-- | SP
pattern R_SP :: X86Reg
pattern R_SP = R_GPR 4 16

-- | BP
pattern R_BP :: X86Reg
pattern R_BP = R_GPR 5 16

-- | SI
pattern R_SI :: X86Reg
pattern R_SI = R_GPR 6 16

-- | DI
pattern R_DI :: X86Reg
pattern R_DI = R_GPR 7 16

-- | R8W
pattern R_R8W :: X86Reg
pattern R_R8W = R_GPR 8 16

-- | R9W
pattern R_R9W :: X86Reg
pattern R_R9W = R_GPR 9 16

-- | R10W
pattern R_R10W :: X86Reg
pattern R_R10W = R_GPR 10 16

-- | R11W
pattern R_R11W :: X86Reg
pattern R_R11W = R_GPR 11 16

-- | R12W
pattern R_R12W :: X86Reg
pattern R_R12W = R_GPR 12 16

-- | R13W
pattern R_R13W :: X86Reg
pattern R_R13W = R_GPR 13 16

-- | R14W
pattern R_R14W :: X86Reg
pattern R_R14W = R_GPR 14 16

-- | R15W
pattern R_R15W :: X86Reg
pattern R_R15W = R_GPR 15 16

------------
-- 32 bit
------------

-- | EAX
pattern R_EAX :: X86Reg
pattern R_EAX = R_GPR 0 32

-- | ECX
pattern R_ECX :: X86Reg
pattern R_ECX = R_GPR 1 32

-- | EDX
pattern R_EDX :: X86Reg
pattern R_EDX = R_GPR 2 32

-- | EBX
pattern R_EBX :: X86Reg
pattern R_EBX = R_GPR 3 32

-- | ESP
pattern R_ESP :: X86Reg
pattern R_ESP = R_GPR 4 32

-- | EBP
pattern R_EBP :: X86Reg
pattern R_EBP = R_GPR 5 32

-- | ESI
pattern R_ESI :: X86Reg
pattern R_ESI = R_GPR 6 32

-- | EDI
pattern R_EDI :: X86Reg
pattern R_EDI = R_GPR 7 32

-- | R8D
pattern R_R8D :: X86Reg
pattern R_R8D = R_GPR 8 32

-- | R9D
pattern R_R9D :: X86Reg
pattern R_R9D = R_GPR 9 32

-- | R10D
pattern R_R10D :: X86Reg
pattern R_R10D = R_GPR 10 32

-- | R11D
pattern R_R11D :: X86Reg
pattern R_R11D = R_GPR 11 32

-- | R12D
pattern R_R12D :: X86Reg
pattern R_R12D = R_GPR 12 32

-- | R13D
pattern R_R13D :: X86Reg
pattern R_R13D = R_GPR 13 32

-- | R14D
pattern R_R14D :: X86Reg
pattern R_R14D = R_GPR 14 32

-- | R15D
pattern R_R15D :: X86Reg
pattern R_R15D = R_GPR 15 32


------------
-- 64 bit
------------

-- | RAX
pattern R_RAX :: X86Reg
pattern R_RAX = R_GPR 0 64

-- | RCX
pattern R_RCX :: X86Reg
pattern R_RCX = R_GPR 1 64

-- | RDX
pattern R_RDX :: X86Reg
pattern R_RDX = R_GPR 2 64

-- | RBX
pattern R_RBX :: X86Reg
pattern R_RBX = R_GPR 3 64

-- | RSP
pattern R_RSP :: X86Reg
pattern R_RSP = R_GPR 4 64

-- | RBP
pattern R_RBP :: X86Reg
pattern R_RBP = R_GPR 5 64

-- | RSI
pattern R_RSI :: X86Reg
pattern R_RSI = R_GPR 6 64

-- | RDI
pattern R_RDI :: X86Reg
pattern R_RDI = R_GPR 7 64

-- | R8
pattern R_R8 :: X86Reg
pattern R_R8 = R_GPR 8 64

-- | R9
pattern R_R9 :: X86Reg
pattern R_R9 = R_GPR 9 64

-- | R10
pattern R_R10 :: X86Reg
pattern R_R10 = R_GPR 10 64

-- | R11
pattern R_R11 :: X86Reg
pattern R_R11 = R_GPR 11 64

-- | R12
pattern R_R12 :: X86Reg
pattern R_R12 = R_GPR 12 64

-- | R13
pattern R_R13 :: X86Reg
pattern R_R13 = R_GPR 13 64

-- | R14
pattern R_R14 :: X86Reg
pattern R_R14 = R_GPR 14 64

-- | R15
pattern R_R15 :: X86Reg
pattern R_R15 = R_GPR 15 64

---------------------------------------------------------------------
-- NAMES
---------------------------------------------------------------------

-- | Register names
registerName :: X86Reg -> String
registerName = \case
   R_AL        -> "al"
   R_BL        -> "bl"
   R_CL        -> "cl"
   R_DL        -> "dl"
   R_AH        -> "ah"
   R_BH        -> "bh"
   R_CH        -> "ch"
   R_DH        -> "dh"
   R_AX        -> "ax"
   R_BX        -> "bx"
   R_CX        -> "cx"
   R_DX        -> "dx"
   R_BP        -> "bp"
   R_SP        -> "sp"
   R_DI        -> "di"
   R_SI        -> "si"
   R_BPL       -> "bpl"
   R_SPL       -> "spl"
   R_DIL       -> "dil"
   R_SIL       -> "sil"
   R_EAX       -> "eax"
   R_EBX       -> "ebx"
   R_ECX       -> "ecx"
   R_EDX       -> "edx"
   R_EBP       -> "ebp"
   R_ESP       -> "esp"
   R_EDI       -> "edi"
   R_ESI       -> "esi"
   R_RAX       -> "rax"
   R_RBX       -> "rbx"
   R_RCX       -> "rcx"
   R_RDX       -> "rdx"
   R_RBP       -> "rbp"
   R_RSP       -> "rsp"
   R_RDI       -> "rdi"
   R_RSI       -> "rsi"
   R_R8        -> "r8"
   R_R9        -> "r9"
   R_R10       -> "r10"
   R_R11       -> "r11"
   R_R12       -> "r12"
   R_R13       -> "r13"
   R_R14       -> "r14"
   R_R15       -> "r15"
   R_R8L       -> "r8l"
   R_R9L       -> "r9l"
   R_R10L      -> "r10l"
   R_R11L      -> "r11l"
   R_R12L      -> "r12l"
   R_R13L      -> "r13l"
   R_R14L      -> "r14l"
   R_R15L      -> "r15l"
   R_R8W       -> "r8w"
   R_R9W       -> "r9w"
   R_R10W      -> "r10w"
   R_R11W      -> "r11w"
   R_R12W      -> "r12w"
   R_R13W      -> "r13w"
   R_R14W      -> "r14w"
   R_R15W      -> "r15w"
   R_R8D       -> "r8d"
   R_R9D       -> "r9d"
   R_R10D      -> "r10d"
   R_R11D      -> "r11d"
   R_R12D      -> "r12d"
   R_R13D      -> "r13d"
   R_R14D      -> "r14d"
   R_R15D      -> "r15d"
   R_ST w      -> "st" ++ show w
   R_CR32 w    -> "cr" ++ show w
   R_CR64 w    -> "cr" ++ show w
   R_DR64 w    -> "dr" ++ show w
   R_MMX w     -> "mm" ++ show w
   R_XMM w     -> "xmm" ++ show w
   R_YMM w     -> "ymm" ++ show w
   R_ZMM w     -> "zmm" ++ show w
   R_CS        -> "cs"
   R_DS        -> "ds"
   R_ES        -> "es"
   R_FS        -> "fs"
   R_GS        -> "gs"
   R_SS        -> "ss"
   R_IP        -> "ip"
   R_EIP       -> "eip"
   R_RIP       -> "rip"
   R_Flags16   -> "flags"
   R_Flags32   -> "eflags"
   R_Flags64   -> "rflags"
   Reg b i s o -> "Reg"++show (b,i,s,o)

