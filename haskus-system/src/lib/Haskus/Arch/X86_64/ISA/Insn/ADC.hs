-- | ADC: add with carry
module Haskus.Arch.X86_64.ISA.Insn.ADC
  ( ADC (..)
  , ADC_AL_i8 (..)
  , ADC_AX_i16 (..)
  , ADC_EAX_i32 (..)
  , ADC_RAX_i32 (..)
  , ADC_r8_i8 (..)
  , ADC_r16_i16 (..)
  , ADC_r32_i32 (..)
  , ADC_r64_i32 (..)
  , ADC_r16_i8 (..)
  , ADC_r32_i8 (..)
  , ADC_r64_i8 (..)
  , ADC_r8_r8 (..)
  , ADC_r16_r16 (..)
  , ADC_r32_r32 (..)
  , ADC_r64_r64 (..)
  , ADC_m8_i8 (..)
  , ADC_m16_i16 (..)
  , ADC_m32_i32 (..)
  , ADC_m64_i32sx (..)
  )
where

import Haskus.Arch.X86_64.ISA.Ops

-- | Add with carry
data ADC
  = ADC_AL_i8_      ADC_AL_i8
  | ADC_AX_i16_     ADC_AX_i16
  | ADC_EAX_i32_    ADC_EAX_i32
  | ADC_RAX_i32_    ADC_RAX_i32
  | ADC_r8_i8_      ADC_r8_i8
  | ADC_r16_i16_    ADC_r16_i16
  | ADC_r32_i32_    ADC_r32_i32
  | ADC_r64_i32_    ADC_r64_i32
  | ADC_r16_i8_     ADC_r16_i8
  | ADC_r32_i8_     ADC_r32_i8
  | ADC_r64_i8_     ADC_r64_i8
  | ADC_r8_r8_      ADC_r8_r8
  | ADC_r16_r16_    ADC_r16_r16
  | ADC_r32_r32_    ADC_r32_r32
  | ADC_r64_r64_    ADC_r64_r64
  | ADC_m8_i8_      ADC_m8_i8
  | ADC_m16_i16_    ADC_m16_i16
  | ADC_m32_i32_    ADC_m32_i32
  | ADC_m64_i32sx_  ADC_m64_i32sx

-- | Add imm8 to AL + CF
newtype ADC_AL_i8 = ADC_AL_i8 Word8

-- | Add imm16 to AX + CF
data ADC_AX_i16 = ADC_AX_i16 !DefaultOperandSize !Word16

-- | Add imm32 to EAX + CF
data ADC_EAX_i32 = ADC_EAX_i32 !DefaultOperandSize !Word32

-- | Add sign-extended imm32 to RAX + CF
newtype ADC_RAX_i32 = ADC_RAX_i32 Word32

-- | Add imm8 to reg8 + CF
data ADC_r8_i8 = ADC_r8_i8 !RegCode !Word8

-- | Add imm16 to reg16 + CF
data ADC_r16_i16 = ADC_r16_i16 !DefaultOperandSize !RegCode !Word16

-- | Add imm32 to reg32 + CF
data ADC_r32_i32 = ADC_r32_i32 !DefaultOperandSize !RegCode !Word32

-- | Add sign-extended imm32 to reg64 + CF
data ADC_r64_i32 = ADC_r64_i32 !RegCode !Word32

-- | Add sign-extended imm8 to reg16 + CF
data ADC_r16_i8 = ADC_r16_i8 !DefaultOperandSize !RegCode !Word8

-- | Add sign-extended imm8 to reg32 + CF
data ADC_r32_i8 = ADC_r32_i8 !DefaultOperandSize !RegCode !Word8

-- | Add sign-extended imm8 to reg64 + CF
data ADC_r64_i8 = ADC_r64_i8 !RegCode !Word8

-- | Add reg8 to reg8 + CF
data ADC_r8_r8 = ADC_r8_r8
  { adc_r8_r8_dst :: !RegCode
  , adc_r8_r8_src :: !RegCode
  , adc_r8_r8_rev :: !ReverseBit
  }

-- | Add reg16 to reg16 + CF
data ADC_r16_r16 = ADC_r16_r16
  { adc_r16_r16_dos :: !DefaultOperandSize
  , adc_r16_r16_dst :: !RegCode
  , adc_r16_r16_src :: !RegCode
  , adc_r16_r16_rev :: !ReverseBit
  }

-- | Add reg32 to reg32 + CF
data ADC_r32_r32 = ADC_r32_r32
  { adc_r32_r32_dos :: !DefaultOperandSize
  , adc_r32_r32_dst :: !RegCode
  , adc_r32_r32_src :: !RegCode
  , adc_r32_r32_rev :: !ReverseBit
  }

-- | Add reg64 to reg64 + CF
data ADC_r64_r64 = ADC_r64_r64
  { adc_r64_r64_dst :: !RegCode
  , adc_r64_r64_src :: !RegCode
  , adc_r64_r64_rev :: !ReverseBit
  }

-- | Add imm8 to mem8 + CF
data ADC_m8_i8 = ADC_m8_i8
  { adc_m8_i8_lock :: !Lock
  , adc_m8_i8_dst  :: !Addr
  , adc_m8_i8_src  :: !Word8
  }

-- | Add imm16 to mem16 + CF
data ADC_m16_i16 = ADC_m16_i16
  { adc_m16_i16_dos  :: !DefaultOperandSize
  , adc_m16_i16_lock :: !Lock
  , adc_m16_i16_dst  :: !Addr
  , adc_m16_i16_src  :: !Word16
  }

-- | Add imm32 to mem32 + CF
data ADC_m32_i32 = ADC_m32_i32
  { adc_m32_i32_dos  :: !DefaultOperandSize
  , adc_m32_i32_lock :: !Lock
  , adc_m32_i32_dst  :: !Addr
  , adc_m32_i32_src  :: !Word32
  }

-- | Add imm32sx to mem64 + CF
data ADC_m64_i32sx = ADC_m64_i32sx
  { adc_m64_i32sx_lock :: !Lock
  , adc_m64_i32sx_dst  :: !Addr
  , adc_m64_i32sx_src  :: !Word32
  }

-- Machine code generation
--------------------------

instance Put ADC_AL_i8 where
  type PutResult ADC_AL_i8 = LocImm8

  put (ADC_AL_i8 v) = do
    oc 0x14
    i8 v

instance Put ADC_AX_i16 where
  type PutResult ADC_AX_i16 = LocImm16

  put (ADC_AX_i16 dos v) = do
    os16 dos
    oc 0x15
    i16 v

instance Put ADC_EAX_i32 where
  type PutResult ADC_EAX_i32 = LocImm32

  put (ADC_EAX_i32 dos v) = do
    os32 dos
    oc 0x15
    i32 v

instance Put ADC_RAX_i32 where
  type PutResult ADC_RAX_i32 = LocImm32sx

  put (ADC_RAX_i32 v) = do
    rexW
    oc 0x15
    i32sx v

instance Put ADC_r8_i8 where
  type PutResult ADC_r8_i8 = LocImm8

  put (ADC_r8_i8 (regRM -> (b,r)) v) = do
    rex W R X b
    oc 0x80
    ocxReg 2 r
    i8 v

instance Put ADC_r16_i16 where
  type PutResult ADC_r16_i16 = LocImm16

  put (ADC_r16_i16 dos (regRM -> (b,r)) v) = do
    os16 dos
    rex W R X b
    oc 0x81
    ocxReg 2 r
    i16 v

instance Put ADC_r32_i32 where
  type PutResult ADC_r32_i32 = LocImm32

  put (ADC_r32_i32 dos (regRM -> (b,r)) v) = do
    os32 dos
    rex W R X b
    oc 0x81
    ocxReg 2 r
    i32 v

instance Put ADC_r64_i32 where
  type PutResult ADC_r64_i32 = LocImm32sx

  put (ADC_r64_i32 (regRM -> (b,r)) v) = do
    rex W1 R X b
    oc 0x81
    ocxReg 2 r
    i32sx v

instance Put ADC_r16_i8 where
  type PutResult ADC_r16_i8 = LocImm8sx

  put (ADC_r16_i8 dos (regRM -> (b,r)) v) = do
    os16 dos
    rex W R X b
    oc 0x83
    ocxReg 2 r
    i8sx v

instance Put ADC_r32_i8 where
  type PutResult ADC_r32_i8 = LocImm8sx

  put (ADC_r32_i8 dos (regRM -> (b,r)) v) = do
    os32 dos
    rex W R X b
    oc 0x83
    ocxReg 2 r
    i8sx v

instance Put ADC_r64_i8 where
  type PutResult ADC_r64_i8 = LocImm8sx

  put (ADC_r64_i8 (regRM -> (b,r)) v) = do
    rex W1 R X b
    oc 0x83
    ocxReg 2 r
    i8sx v

instance Put ADC_r8_r8 where
  type PutResult ADC_r8_r8 = ()

  put (ADC_r8_r8 dst src rev) = do
    let (r, b, modrm, o) = revRegs rev 0x10 dst src
    rex W r X b
    oc o
    putModRM modrm

instance Put ADC_r16_r16 where
  type PutResult ADC_r16_r16 = ()

  put (ADC_r16_r16 dos dst src rev) = do
    let (r, b, modrm, o) = revRegs rev 0x11 dst src
    os16 dos
    rex W r X b
    oc o
    putModRM modrm

instance Put ADC_r32_r32 where
  type PutResult ADC_r32_r32 = ()

  put (ADC_r32_r32 dos dst src rev) = do
    let (r, b, modrm, o) = revRegs rev 0x11 dst src
    os32 dos
    rex W r X b
    oc o
    putModRM modrm

instance Put ADC_r64_r64 where
  type PutResult ADC_r64_r64 = ()

  put (ADC_r64_r64 dst src rev) = do
    let (r, b, modrm, o) = revRegs rev 0x11 dst src
    rex W1 r X b
    oc o
    putModRM modrm

instance Put ADC_m8_i8 where
  type PutResult ADC_m8_i8 = (LocDispMaybe, LocImm8)

  put (ADC_m8_i8 lock m v) = do
    let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
    lockMaybe lock
    segMaybe mseg
    addrSizeMaybe masize
    rex W R x b
    oc 0x80
    ocxMem 2 m_mod m_rm
    sibMaybe msib
    loc_disp <- dispMaybe disp
    loc_imm <- i8 v
    pure (loc_disp, loc_imm)

instance Put ADC_m16_i16 where
  type PutResult ADC_m16_i16 = (LocDispMaybe, LocImm16)

  put (ADC_m16_i16 dos lock m v) = do
    let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
    lockMaybe lock
    segMaybe mseg
    addrSizeMaybe masize
    os16 dos
    rex W R x b
    oc 0x81
    ocxMem 2 m_mod m_rm
    sibMaybe msib
    loc_disp <- dispMaybe disp
    loc_imm <- i16 v
    pure (loc_disp, loc_imm)

instance Put ADC_m32_i32 where
  type PutResult ADC_m32_i32 = (LocDispMaybe, LocImm32)

  put (ADC_m32_i32 dos lock m v) = do
    let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
    lockMaybe lock
    segMaybe mseg
    addrSizeMaybe masize
    os32 dos
    rex W R x b
    oc 0x81
    ocxMem 2 m_mod m_rm
    sibMaybe msib
    loc_disp <- dispMaybe disp
    loc_imm <- i32 v
    pure (loc_disp, loc_imm)

instance Put ADC_m64_i32sx where
  type PutResult ADC_m64_i32sx = (LocDispMaybe, LocImm32sx)

  put (ADC_m64_i32sx lock m v) = do
    let (mseg, masize, m_mod, m_rm, msib, disp, x, b) = addrFields m
    lockMaybe lock
    segMaybe mseg
    addrSizeMaybe masize
    rex W1 R x b
    oc 0x81
    ocxMem 2 m_mod m_rm
    sibMaybe msib
    loc_disp <- dispMaybe disp
    loc_imm <- i32sx v
    pure (loc_disp, loc_imm)

