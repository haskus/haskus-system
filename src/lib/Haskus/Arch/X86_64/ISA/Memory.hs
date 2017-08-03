{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}

-- | Memory operand
module Haskus.Arch.X86_64.ISA.Memory
   ( Addr (..)
   , AlignConstraint (..)
   , AddrFam (..)
   , SegFam (..)
   , Scale (..)
   , MemType (..)
   , X86MemFamP
   , X86MemFamT
   , X86MemFam
   , X86Mem
   , addrFamToAddr
   , baseDefaultSegment
   , emptyAddrFam
   , x86memFamToMem
   -- * Memory families
   , memFamFixed
   , memFamOpSize
   , memFamW
   , memFamL
   , memFamOpSizePair
   , memFamPair16o32
   , memFamVoid
   , memFamPtr
   , memFamDescTable
   , memFamFixedFP
   , memFamFP
   , memFamFPUInt
   , memFamBCD
   , memFamFPUEnv
   , memFamFPUState
   , memFamStringSource
   , memFamStringDest
   , memFamdSrDI
   , memFamState
   , memFamVSIB
   , memFamVSIBxy
   , memFamDSrAX
   , memFamRelCode8
   , memFamRelCode16o32
   , memFamOffset
   )
where

import Haskus.Arch.X86_64.ISA.Register
import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Arch.Common.Memory
import Haskus.Arch.Common.Register
import Haskus.Utils.Solver
import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Format.Binary.Word

import Control.Applicative

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
   { addrSeg       :: !(Maybe X86Reg)          -- ^ Segment register
   , addrBase      :: !(Maybe X86Reg)          -- ^ Base register
   , addrIndex     :: !(Maybe X86Reg)          -- ^ Index register
   , addrIndexSize :: !(Maybe Size)            -- ^ Index size if index register is a vector
   , addrScale     :: !(Maybe Scale)           -- ^ Scale
   , addrDisp      :: !(Maybe SizedValue)      -- ^ Displacement
   , addrAlign     :: !(Maybe AlignConstraint) -- ^ Alignment constraint
   }
   deriving (Show,Eq)

data AlignConstraint
   = StrictAlign    {-# UNPACK #-} !Word
   | PreferredAlign {-# UNPACK #-} !Word
   deriving (Show,Eq)


-- | A memory address family
data AddrFam = AddrFam
   { addrFamSeg       :: !(Maybe SegFam)          -- ^ Segment register
   , addrFamBase      :: !(Maybe X86Reg)          -- ^ Base register
   , addrFamIndex     :: !(Maybe X86RegFamT)      -- ^ Index register
   , addrFamIndexSize :: !(Maybe Size)            -- ^ Index size if index register is a vector
   , addrFamScale     :: !(Maybe Scale)           -- ^ Scale
   , addrFamDisp      :: !(Maybe Word64)          -- ^ Displacement
   , addrFamDispSize  :: !(Maybe Size)            -- ^ Displacement size
   , addrFamAlign     :: !(Maybe AlignConstraint) -- ^ Alignment constraint
   }
   deriving (Show,Eq)

-- | Empty address family
emptyAddrFam :: AddrFam
emptyAddrFam = AddrFam Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Convert an address family representing a single address into an address
addrFamToAddr :: Maybe X86Reg -> AddrFam -> Maybe Addr
addrFamToAddr seg AddrFam{..}
   | isNothing addrFamBase
     && isNothing addrFamIndex
     && isNothing addrFamDisp                      = Nothing
   | isJust addrFamDispSize /= isJust addrFamDisp  = Nothing
   | fmap regFamToReg addrFamIndex == Just Nothing = Nothing
   | otherwise = Just <| Addr
         { addrSeg = case addrFamSeg of
               -- default: infer from the base register or override it
               Nothing                 -> seg <|> fmap baseDefaultSegment addrFamBase
               -- fixed: just use it
               Just (FixedSeg r)       -> Just r
               -- overridable: use the provided seg or the default one
               Just (OverridableSeg r) -> seg <|> Just r
            
         , addrBase      = addrFamBase
         , addrIndex     = fmap regFamToReg' addrFamIndex
         , addrIndexSize = addrFamIndexSize
         , addrScale     = addrFamScale
         , addrAlign     = addrFamAlign
         , addrDisp      = toSizedValue <$> addrFamDispSize <*> addrFamDisp
         }

-- | Give the default segment for the given base register
baseDefaultSegment :: X86Reg -> X86Reg
baseDefaultSegment = \case
   R_BP  -> R_SS
   R_EBP -> R_SS
   R_RBP -> R_SS
   R_RIP -> R_CS
   R_EIP -> R_CS
   R_IP  -> R_CS
   _     -> R_DS

-- | Family of segment registers
data SegFam
   = FixedSeg X86Reg       -- ^ Fixed segment register
   | OverridableSeg X86Reg -- ^ Overridable segment register
   deriving (Show,Eq)


-- | SIB scale factor
data Scale
   = Scale1 
   | Scale2 
   | Scale4 
   | Scale8 
   deriving (Show,Eq)


-- | Memory address type
data MemType
   = T_MemGeneric      -- ^ generic (Int, Vector, etc.)
   | T_MemPair MemType -- ^ Pair
   | T_MemVoid         -- ^ The pointer is used to identify a page, etc. (e.g., CLFLUSH)
   | T_MemPtr          -- ^ m16:16, m16:32 or m16:64 (16-bit selector + offset)
   | T_MemDescTable    -- ^ m16:16, m16:32 or m16:64 (16-bit selector + offset)
   | T_MemFP           -- ^ IEEE Floating-Point
   | T_MemBCD          -- ^ Binary coded decimal
   | T_MemFPUEnv       -- ^ FPU environment
   | T_MemFPUState     -- ^ FPU state
   | T_MemState        -- ^ Processor extended states (cf XSAVE/XRSTOR)
   | T_MemVSIB         -- ^ generic memory referred to by indices in a vector register (indicated in the VSIB)
   | T_MemRel          -- ^ Relative code offset
   | T_MemOffset       -- ^ Offset from segment base
   deriving (Show,Eq)

-- | Convert a memory family to a memory
x86memFamToMem :: X86MemFamT -> Maybe X86Mem
x86memFamToMem m = do
   -- memFamToMem doesn't handle addrFam to Addr for now
   -- so we do it explicitly here
   m' <- memFamToMem m
   a' <- addrFamToAddr Nothing (memAddr m')
   return (Mem a' (memType m') (memSize m'))

---------------------------------------
-- Memory families
---------------------------------------

-- | Predicated memory family
type X86MemFamP  = MemFamP X86Pred String AddrFam MemType

-- | Memory family
type X86MemFamT  = MemFamT AddrFam MemType

-- | Memory family
type X86MemFam t = MemFam t AddrFam MemType

-- | Memory
type X86Mem      = Mem Addr MemType

-- | Fixed size family
memFamFixed :: Word -> X86MemFamP
memFamFixed sz = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemGeneric
   , memFamSize = Terminal (Just sz)
   }

-- | Operand-sized
memFamOpSize :: X86MemFamP
memFamOpSize = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemGeneric
   , memFamSize = pOpSize64 (Just 8) (Just 16) (Just 32) (Just 64)
   }

-- | Generic depending on REX.W
memFamW :: Word -> Word -> X86MemFamP
memFamW s1 s2 = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemGeneric
   , memFamSize = NonTerminal
      [ (Not (Predicate (PrefixPred PrefixW)), Terminal $ Just s1)
      , (    (Predicate (PrefixPred PrefixW)), Terminal $ Just s2)
      ]
   }

-- | Generic depending on L
memFamL :: Word -> Word -> X86MemFamP
memFamL s1 s2 = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemGeneric
   , memFamSize = NonTerminal
      [ (Not (Predicate (PrefixPred PrefixL)), Terminal $ Just s1)
      , (    (Predicate (PrefixPred PrefixL)), Terminal $ Just s2)
      ]
   }

-- | Pair of operand-sized
memFamOpSizePair :: X86MemFamP
memFamOpSizePair = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal (T_MemPair T_MemGeneric)
   , memFamSize = pOpSize64 (Just 16) (Just 32) (Just 64) (Just 128)
   }

-- | Pair of 16- or 32-bit
memFamPair16o32 :: X86MemFamP
memFamPair16o32 = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal (T_MemPair T_MemGeneric)
   , memFamSize = NonTerminal
      [ (pOverriddenOperationSize64 OpSize16, Terminal $ Just 32)
      , (pOverriddenOperationSize64 OpSize32, Terminal $ Just 64)
      ]
   }


-- | Void
memFamVoid :: X86MemFamP
memFamVoid = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemVoid
   , memFamSize = Terminal (Just 0)
   }

-- | Indirect pointer: 16 bit selector + 16/32 offset
memFamPtr :: X86MemFamP
memFamPtr = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemPtr
   , memFamSize = NonTerminal
      [ (pOverriddenOperationSize64 OpSize16, Terminal $ Just 16)
      , (pOverriddenOperationSize64 OpSize32, Terminal $ Just 32)
      , (pOverriddenOperationSize64 OpSize64, Terminal $ Just 64)
      ]
   }

-- | Descriptor table: 4/8-byte address + 2-byte limit
memFamDescTable :: X86MemFamP
memFamDescTable = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemDescTable
   , memFamSize = NonTerminal
      [ (pMode64bit    , Terminal $ Just 10)
      , (Not pMode64bit, Terminal $ Just 6)
      ]
   }

-- | Fixed Floating-Point
memFamFixedFP :: Word -> X86MemFamP
memFamFixedFP sz = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemFP
   , memFamSize = Terminal (Just sz)
   }

-- | 32/64 Floating-Point depending on the FPUSize opcode bit
memFamFP :: X86MemFamP
memFamFP = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemFP
   , memFamSize = NonTerminal
      [ (Not pFPUSizeBit, Terminal $ Just 32)
      , (    pFPUSizeBit, Terminal $ Just 64)
      ]
   }

-- | 16/32 int depending on the FPUSize opcode bit
memFamFPUInt :: X86MemFamP
memFamFPUInt = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemGeneric
   , memFamSize = NonTerminal
      [ (Not pFPUSizeBit, Terminal $ Just 32)
      , (    pFPUSizeBit, Terminal $ Just 16)
      ]
   }

-- | 80-bit binary coded decimal
memFamBCD :: X86MemFamP
memFamBCD = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemBCD
   , memFamSize = Terminal (Just 80)
   }

-- | FPU environment
memFamFPUEnv :: X86MemFamP
memFamFPUEnv = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemFPUEnv
   , memFamSize = NonTerminal
      [ (pOverriddenOperationSize OpSize16, Terminal $ Just (14*8))
      , (pOverriddenOperationSize OpSize32, Terminal $ Just (28*8))
      ]
   }

-- | FPU state
memFamFPUState :: X86MemFamP
memFamFPUState = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemFPUState
   , memFamSize = NonTerminal
      [ (pOverriddenOperationSize OpSize16, Terminal $ Just ((80+14)*8))
      , (pOverriddenOperationSize OpSize32, Terminal $ Just ((80+28)*8))
      ]
   }

-- | Operand-size at DS:rSI
memFamStringSource :: X86MemFamP
memFamStringSource = MemFam
   { memFamAddr = NonTerminal
      [ (pOverriddenAddressSize AddrSize16, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (FixedSeg R_DS)
            , addrFamBase = Just R_SI
            })
      , (pOverriddenAddressSize AddrSize32, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (FixedSeg R_DS)
            , addrFamBase = Just R_ESI
            })
      , (pOverriddenAddressSize AddrSize64, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (FixedSeg R_DS)
            , addrFamBase = Just R_RSI
            })
      ]
   , memFamType = Terminal T_MemGeneric
   , memFamSize = pOpSize64 (Just 8) (Just 16) (Just 32) (Just 64)
   }

-- | Operand-size at ES:rDI
memFamStringDest :: X86MemFamP
memFamStringDest = MemFam
   { memFamAddr = NonTerminal
      [ (pOverriddenAddressSize AddrSize16, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (FixedSeg R_ES)
            , addrFamBase = Just R_DI
            })
      , (pOverriddenAddressSize AddrSize32, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (FixedSeg R_ES)
            , addrFamBase = Just R_EDI
            })
      , (pOverriddenAddressSize AddrSize64, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (FixedSeg R_ES)
            , addrFamBase = Just R_RDI
            })
      ]
   , memFamType = Terminal T_MemGeneric
   , memFamSize = pOpSize64 (Just 8) (Just 16) (Just 32) (Just 64)
   }

-- | Operand-size at dS:rDI (DS is overridable)
memFamdSrDI :: X86MemFamP
memFamdSrDI = MemFam
   { memFamAddr = NonTerminal
      [ (pOverriddenAddressSize AddrSize16, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (OverridableSeg R_DS)
            , addrFamBase = Just R_DI
            })
      , (pOverriddenAddressSize AddrSize32, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (OverridableSeg R_DS)
            , addrFamBase = Just R_EDI
            })
      , (pOverriddenAddressSize AddrSize64, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (OverridableSeg R_DS)
            , addrFamBase = Just R_RDI
            })
      ]
   , memFamType = Terminal T_MemGeneric
   , memFamSize = Terminal $ Just (16*8)
   }

-- | Processor extended states
memFamState :: X86MemFamP
memFamState = MemFam
   { memFamAddr = Terminal Nothing
   , memFamType = Terminal T_MemState
   , memFamSize = Terminal Nothing
   }

-- | VSIB memory
memFamVSIB :: Word -> Size -> Size -> X86MemFamP
memFamVSIB sz idxsz vecsz = MemFam
   { memFamAddr = Terminal $ Just addr
   , memFamType = Terminal T_MemVSIB
   , memFamSize = Terminal (Just sz)
   }
   where
      addr = emptyAddrFam
         { addrFamSeg       = Just (FixedSeg R_DS)
         , addrFamIndex     = Just idx
         , addrFamIndexSize = Just idxsz
         }
      s = case vecsz of
            Size128 -> R_XMM 0
            Size256 -> R_YMM 0
            -- Size512 -> R_ZMM 0
            e       -> error ("Invalid VSIB vector size: " ++ show e)
      idx = (regFamFromReg s)
               { regFamId   = Any
               }

-- | VSIB memory: L chooses between XMM and YMM
memFamVSIBxy :: Word -> Size -> X86MemFamP
memFamVSIBxy sz idxsz = MemFam
   { memFamAddr = sPrefix PrefixL (Terminal $ Just addrX) (Terminal $ Just addrY)
   , memFamType = Terminal T_MemVSIB
   , memFamSize = Terminal (Just sz)
   }
   where
      addr = emptyAddrFam
         { addrFamSeg       = Just (FixedSeg R_DS)
         , addrFamIndexSize = Just idxsz
         }
      addrX = addr
         { addrFamIndex     = Just idxX
         }
      addrY = addr
         { addrFamIndex     = Just idxY
         }
      idxX = (regFamFromReg (R_XMM 0))
               { regFamId   = Any
               }
      idxY = (regFamFromReg (R_YMM 0))
               { regFamId   = Any
               }
            
-- | Address in DS:RAX/EAX/AX (cf MONITOR)
memFamDSrAX :: X86MemFamP
memFamDSrAX = MemFam
   { memFamAddr = NonTerminal
      [ (pOverriddenAddressSize AddrSize16, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (OverridableSeg R_DS)
            , addrFamBase = Just R_AX
            })
      , (pOverriddenAddressSize AddrSize32, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (OverridableSeg R_DS)
            , addrFamBase = Just R_EAX
            })
      , (pOverriddenAddressSize AddrSize64, Terminal $ Just $ emptyAddrFam
            { addrFamSeg  = Just (OverridableSeg R_DS)
            , addrFamBase = Just R_RAX
            })
      ]
   , memFamType = Terminal T_MemVoid
   , memFamSize = Terminal (Just 0)
   }

-- | Mem relative code
memFamRelCode8 :: X86MemFamP
memFamRelCode8 = MemFam
   { memFamAddr = orderedNonTerminal
      [ (pMode64bit, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (FixedSeg R_CS)
            , addrFamBase     = Just R_RIP
            , addrFamDispSize = Just Size8
            })
      , (pOverriddenOperationSize OpSize16, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (FixedSeg R_CS)
            , addrFamBase     = Just R_IP
            , addrFamDispSize = Just Size8
            })
      , (pOverriddenOperationSize OpSize32, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (FixedSeg R_CS)
            , addrFamBase     = Just R_EIP
            , addrFamDispSize = Just Size8
            })
      ]
   , memFamType = Terminal T_MemRel
   , memFamSize = Terminal (Just 0)
   }


-- | Mem relative code
memFamRelCode16o32 :: X86MemFamP
memFamRelCode16o32 = MemFam
   { memFamAddr = orderedNonTerminal
      [ (pMode64bit, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (FixedSeg R_CS)
            , addrFamBase     = Just R_RIP
            , addrFamDispSize = Just Size32
            })
      , (pOverriddenOperationSize OpSize16, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (FixedSeg R_CS)
            , addrFamBase     = Just R_IP
            , addrFamDispSize = Just Size16
            })
      , (pOverriddenOperationSize OpSize32, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (FixedSeg R_CS)
            , addrFamBase     = Just R_EIP
            , addrFamDispSize = Just Size32
            })
      ]
   , memFamType = Terminal T_MemRel
   , memFamSize = Terminal (Just 0)
   }

-- | Mem offset
--
-- Address size specifies the size of the offset (16,32,64)
-- Operand size specifies the size of the data (8,16,32,64)
memFamOffset :: X86MemFamP
memFamOffset = MemFam
   { memFamAddr = NonTerminal
      [ (pOverriddenAddressSize AddrSize16, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (OverridableSeg R_DS)
            , addrFamDispSize = Just Size16
            })
      , (pOverriddenAddressSize AddrSize32, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (OverridableSeg R_DS)
            , addrFamDispSize = Just Size32
            })
      , (pOverriddenAddressSize AddrSize64, Terminal $ Just $ emptyAddrFam
            { addrFamSeg      = Just (OverridableSeg R_DS)
            , addrFamDispSize = Just Size64
            })
      ]
   , memFamType = Terminal T_MemOffset
   , memFamSize = pOpSize64 (Just 8) (Just 16) (Just 32) (Just 64)
   }
