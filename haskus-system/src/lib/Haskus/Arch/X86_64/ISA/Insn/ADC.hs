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
  , ADC_r8_m8 (..)
  , ADC_r16_m16 (..)
  , ADC_r32_m32 (..)
  , ADC_r64_m64 (..)
  , ADC_m8_r8 (..)
  , ADC_m16_r16 (..)
  , ADC_m32_r32 (..)
  , ADC_m64_r64 (..)
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
  | ADC_r8_m8_      ADC_r8_m8
  | ADC_r16_m16_    ADC_r16_m16
  | ADC_r32_m32_    ADC_r32_m32
  | ADC_r64_m64_    ADC_r64_m64
  | ADC_m8_r8_      ADC_m8_r8
  | ADC_m16_r16_    ADC_m16_r16
  | ADC_m32_r32_    ADC_m32_r32
  | ADC_m64_r64_    ADC_m64_r64

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

-- | Add reg8 to mem8 + CF
data ADC_m8_r8 = ADC_m8_r8
  { adc_m8_r8_lock :: !Lock
  , adc_m8_r8_dst  :: !Addr
  , adc_m8_r8_src  :: !RegCode
  }

-- | Add reg16 to mem16 + CF
data ADC_m16_r16 = ADC_m16_r16
  { adc_m16_r16_dos  :: !DefaultOperandSize
  , adc_m16_r16_lock :: !Lock
  , adc_m16_r16_dst  :: !Addr
  , adc_m16_r16_src  :: !RegCode
  }

-- | Add reg32 to mem32 + CF
data ADC_m32_r32 = ADC_m32_r32
  { adc_m32_r32_dos  :: !DefaultOperandSize
  , adc_m32_r32_lock :: !Lock
  , adc_m32_r32_dst  :: !Addr
  , adc_m32_r32_src  :: !RegCode
  }

-- | Add reg64 to mem64 + CF
data ADC_m64_r64 = ADC_m64_r64
  { adc_m64_r64_lock :: !Lock
  , adc_m64_r64_dst  :: !Addr
  , adc_m64_r64_src  :: !RegCode
  }

-- | Add mem8 to reg8 + CF
data ADC_r8_m8 = ADC_r8_m8
  { adc_r8_m8_dst  :: !RegCode
  , adc_r8_m8_src  :: !Addr
  }

-- | Add mem16 to reg16 + CF
data ADC_r16_m16 = ADC_r16_m16
  { adc_r16_m16_dos  :: !DefaultOperandSize
  , adc_r16_m16_dst  :: !RegCode
  , adc_r16_m16_src  :: !Addr
  }

-- | Add mem32 to reg32 + CF
data ADC_r32_m32 = ADC_r32_m32
  { adc_r32_m32_dos  :: !DefaultOperandSize
  , adc_r32_m32_dst  :: !RegCode
  , adc_r32_m32_src  :: !Addr
  }

-- | Add mem64 to reg64 + CF
data ADC_r64_m64 = ADC_r64_m64
  { adc_r64_m64_dst  :: !RegCode
  , adc_r64_m64_src  :: !Addr
  }

-- Machine code generation
--------------------------

instance Put ADC_AL_i8 where
  type PutResult ADC_AL_i8    = LocImm8
  put (ADC_AL_i8 v)           = gen_al_i8 0x14 v

instance Put ADC_AX_i16 where
  type PutResult ADC_AX_i16   = LocImm16
  put (ADC_AX_i16 dos v)      = gen_ax_i16 0x15 dos v

instance Put ADC_EAX_i32 where
  type PutResult ADC_EAX_i32  = LocImm32
  put (ADC_EAX_i32 dos v)     = gen_eax_i32 0x15 dos v

instance Put ADC_RAX_i32 where
  type PutResult ADC_RAX_i32  = LocImm32sx
  put (ADC_RAX_i32 v)         = gen_rax_i32sx 0x15 v

instance Put ADC_r8_i8 where
  type PutResult ADC_r8_i8    = LocImm8
  put (ADC_r8_i8 r v)         = gen_r8_i8 0x80 2 r v

instance Put ADC_r16_i16 where
  type PutResult ADC_r16_i16  = LocImm16
  put (ADC_r16_i16 dos r v)   = gen_r16_i16 0x81 2 dos r v

instance Put ADC_r32_i32 where
  type PutResult ADC_r32_i32  = LocImm32
  put (ADC_r32_i32 dos r v)   = gen_r32_i32 0x81 2 dos r v

instance Put ADC_r64_i32 where
  type PutResult ADC_r64_i32  = LocImm32sx
  put (ADC_r64_i32 r v)       = gen_r64_i32sx 0x81 2 r v

instance Put ADC_r16_i8 where
  type PutResult ADC_r16_i8   = LocImm8sx
  put (ADC_r16_i8 dos r v)    = gen_r16_i8sx 0x83 2 dos r v

instance Put ADC_r32_i8 where
  type PutResult ADC_r32_i8   = LocImm8sx
  put (ADC_r32_i8 dos r v)    = gen_r32_i8sx 0x83 2 dos r v

instance Put ADC_r64_i8 where
  type PutResult ADC_r64_i8   = LocImm8sx
  put (ADC_r64_i8 r v)        = gen_r64_i8sx 0x83 2 r v

instance Put ADC_r8_r8 where
  type PutResult ADC_r8_r8    = ()
  put (ADC_r8_r8 dst src rev) = gen_r8_r8_rev 0x10 dst src rev

instance Put ADC_r16_r16 where
  type PutResult ADC_r16_r16        = ()
  put (ADC_r16_r16 dos dst src rev) = gen_r16_r16_rev 0x11 dos dst src rev

instance Put ADC_r32_r32 where
  type PutResult ADC_r32_r32        = ()
  put (ADC_r32_r32 dos dst src rev) = gen_r32_r32_rev 0x11 dos dst src rev

instance Put ADC_r64_r64 where
  type PutResult ADC_r64_r64    = ()
  put (ADC_r64_r64 dst src rev) = gen_r64_r64_rev 0x11 dst src rev

instance Put ADC_m8_i8 where
  type PutResult ADC_m8_i8       = (LocDispMaybe, LocImm8)
  put (ADC_m8_i8 lock m v)       = gen_m8_i8 0x80 2 lock m v

instance Put ADC_m16_i16 where
  type PutResult ADC_m16_i16     = (LocDispMaybe, LocImm16)
  put (ADC_m16_i16 dos lock m v) = gen_m16_i16 0x81 2 dos lock m v

instance Put ADC_m32_i32 where
  type PutResult ADC_m32_i32     = (LocDispMaybe, LocImm32)
  put (ADC_m32_i32 dos lock m v) = gen_m32_i32 0x81 2 dos lock m v

instance Put ADC_m64_i32sx where
  type PutResult ADC_m64_i32sx   = (LocDispMaybe, LocImm32sx)
  put (ADC_m64_i32sx lock m v)   = gen_m64_i32sx 0x81 2 lock m v

instance Put ADC_m8_r8 where
  type PutResult ADC_m8_r8       = LocDispMaybe
  put (ADC_m8_r8 lock m r)       = gen_m8_r8 0x10 lock m r

instance Put ADC_m16_r16 where
  type PutResult ADC_m16_r16     = LocDispMaybe
  put (ADC_m16_r16 dos lock m r) = gen_m16_r16 0x11 dos lock m r

instance Put ADC_m32_r32 where
  type PutResult ADC_m32_r32     = LocDispMaybe
  put (ADC_m32_r32 dos lock m r) = gen_m32_r32 0x11 dos lock m r

instance Put ADC_m64_r64 where
  type PutResult ADC_m64_r64     = LocDispMaybe
  put (ADC_m64_r64 lock m r)     = gen_m64_r64 0x11 lock m r

instance Put ADC_r8_m8 where
  type PutResult ADC_r8_m8       = LocDispMaybe
  put (ADC_r8_m8 r m)            = gen_r8_m8 0x12 r m

instance Put ADC_r16_m16 where
  type PutResult ADC_r16_m16     = LocDispMaybe
  put (ADC_r16_m16 dos r m)      = gen_r16_m16 0x13 dos r m

instance Put ADC_r32_m32 where
  type PutResult ADC_r32_m32     = LocDispMaybe
  put (ADC_r32_m32 dos r m)      = gen_r32_m32 0x13 dos r m

instance Put ADC_r64_m64 where
  type PutResult ADC_r64_m64     = LocDispMaybe
  put (ADC_r64_m64 r m)          = gen_r64_m64 0x13 r m
