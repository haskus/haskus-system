{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Operand
module Haskus.Arch.X86_64.ISA.Operand
   ( OperandFam(..)
   , OperandFamT
   , OperandFamP
   , OperandStorage(..)
   , OperandSpec (..)
   , OperandSpecP
   , OperandSpecT
   , AccessMode (..)
   , Operand(..)
   , Addr(..)
   , SubRegType (..)
   , MemType (..)
   , isImmediate
   , opFamToOp
   )
where

import Haskus.Arch.Common.Solver
import Haskus.Arch.Common.Immediate
import Haskus.Arch.Common.Register
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Arch.X86_64.ISA.Register
import Haskus.Arch.X86_64.ISA.Immediate
import Haskus.Arch.X86_64.ISA.Memory
import Haskus.Utils.Solver
import Haskus.Utils.Flow
import Haskus.Utils.List (nub)



-------------------------------------------------------------------
-- Operands
-------------------------------------------------------------------

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
-- (XMM or YMM). It can be ignored or fixed at a specified value.
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

-- | An operand
data Operand
   = OpImm X86Imm            -- ^ Immediate value
   | OpReg X86Reg            -- ^ Register
   | OpMem X86Mem            -- ^ Memory address
   | OpRegPair X86Reg X86Reg -- ^ REG:REG
   | OpImmPair X86Imm X86Imm -- ^ Immediate 16:16, 16:32 or 16:8 (cf ENTER)
   deriving (Show,Eq)

-- | Operand family
data OperandFam t
   = T_Mem (X86MemFam t)                  -- ^ Memory address
   | T_Reg (X86RegFam t)                  -- ^ Register
   | T_Imm (X86ImmFam t)                  -- ^ Immediate value
   | T_Pair (OperandFam t) (OperandFam t) -- ^ Pair (AAA:BBB) of immediates or registers

deriving instance (Show (OperandFam T))
deriving instance (Show (OperandFam (NT X86Pred X86Err)))

deriving instance (Eq (OperandFam T))
deriving instance (Eq (OperandFam (NT X86Pred X86Err)))

deriving instance (Ord (OperandFam T))
deriving instance (Ord (OperandFam (NT X86Pred X86Err)))

-- | Predicated operand type
type OperandFamP = OperandFam (NT X86Pred X86Err)

-- | Terminal operand type
type OperandFamT = OperandFam T

instance Predicated (OperandFam (NT X86Pred X86Err)) where
   type Pred     (OperandFam (NT X86Pred X86Err)) = X86Pred
   type PredErr  (OperandFam (NT X86Pred X86Err)) = X86Err
   type PredTerm (OperandFam (NT X86Pred X86Err)) = OperandFam T

   liftTerminal = \case
      T_Pair x y   -> T_Pair (liftTerminal x) (liftTerminal y)
      T_Mem x      -> T_Mem  (liftTerminal x)
      T_Reg x      -> T_Reg  (liftTerminal x)
      T_Imm x      -> T_Imm  (liftTerminal x)

   reducePredicates oracle = \case
      -- We allow the first element of the pair not to be reducible, in which
      -- case it's not a pair. We do this to support the family:
      --    AX, DX:AX, EDX:RAX, RDX:RAX
      T_Pair x@(T_Reg {}) y@(T_Reg {})   ->
         case reducePredicates oracle x of
            Match _ -> initP T_Pair T_Pair
                        |> (`applyP` reducePredicates oracle x)
                        |> (`applyP` reducePredicates oracle y)
                        |> resultP
            _       -> reducePredicates oracle y
      T_Pair x y   -> initP T_Pair T_Pair
                        |> (`applyP` reducePredicates oracle x)
                        |> (`applyP` reducePredicates oracle y)
                        |> resultP
      T_Mem x      -> initP T_Mem T_Mem
                        |> (`applyP` reducePredicates oracle x)
                        |> resultP
      T_Reg x      -> initP T_Reg T_Reg
                        |> (`applyP` reducePredicates oracle x)
                        |> resultP
      T_Imm x      -> initP T_Imm T_Imm
                        |> (`applyP` reducePredicates oracle x)
                        |> resultP

   simplifyPredicates oracle = \case
      T_Pair x y   -> T_Pair (simplifyPredicates oracle x)
                             (simplifyPredicates oracle y)
      T_Mem x      -> T_Mem (simplifyPredicates oracle x)
      T_Reg x      -> T_Reg (simplifyPredicates oracle x)
      T_Imm x      -> T_Imm (simplifyPredicates oracle x)

   getTerminals = \case
      T_Pair xs ys  -> [ T_Pair x y   | x <- getTerminals xs
                                      , y <- getTerminals ys
                       ]
      T_Mem xs      -> [ T_Mem x      | x <- getTerminals xs ]
      T_Reg xs      -> [ T_Reg x      | x <- getTerminals xs ]
      T_Imm xs      -> [ T_Imm x      | x <- getTerminals xs ]

   getPredicates = \case
      T_Pair xs ys  -> nub $ concat [ getPredicates xs, getPredicates ys ]
      T_Mem xs      -> getPredicates xs
      T_Reg xs      -> getPredicates xs
      T_Imm xs      -> getPredicates xs




-- | Operand storage
data OperandStorage
   = S_RM         -- ^ Operand stored in ModRM.rm (+ (V)SIB)
   | S_Reg        -- ^ Operand stored in ModRM.reg
   | S_Imm        -- ^ Operand stored in immediate bytes
   | S_Imm8h      -- ^ Operand stored in bits [7:4] of the immediate byte
   | S_Imm8l      -- ^ Operand stored in bits [3:0] of the immediate byte
   | S_Implicit   -- ^ Implicit
   | S_Vvvv       -- ^ Operand stored in Vex.vvvv field
   | S_OpcodeLow3 -- ^ Operand stored in opcode 3 last bits
   deriving (Show,Eq)

-- | Operand specification (parameterized)
data OperandSpec t = OperandSpec
   { opMode   :: !AccessMode
   , opFam    :: !(Q t (OperandFam t))
   , opStore  :: !OperandStorage
   }

deriving instance (Show (OperandSpec T))
deriving instance (Show (OperandSpec (NT X86Pred X86Err)))

-- | Predicated operand spec
type OperandSpecP = OperandSpec (NT X86Pred X86Err)

-- | Operand specification
type OperandSpecT  = OperandSpec T

instance Predicated (OperandSpec (NT X86Pred X86Err)) where
   type Pred     (OperandSpec (NT X86Pred X86Err)) = X86Pred
   type PredErr  (OperandSpec (NT X86Pred X86Err)) = X86Err
   type PredTerm (OperandSpec (NT X86Pred X86Err)) = OperandSpec T

   liftTerminal (OperandSpec m t s) =
      OperandSpec m (liftTerminal (liftTerminal t)) s

   reducePredicates oracle (OperandSpec m t s) =
      case reducePredicates oracle t of
         NoMatch         -> NoMatch
         MatchDiverge xs -> MatchDiverge ((\x -> OperandSpec m x s) <$> xs)
         MatchFail es    -> MatchFail es
         DontMatch a     -> DontMatch (OperandSpec m a s)
         Match a         -> case reducePredicates oracle a of
            NoMatch         -> NoMatch
            MatchDiverge xs -> MatchDiverge ((\x -> OperandSpec m (liftTerminal x) s) <$> xs)
            MatchFail es    -> MatchFail es
            DontMatch b     -> DontMatch (OperandSpec m (liftTerminal b) s)
            Match b         -> Match (OperandSpec m b s)

   simplifyPredicates oracle (OperandSpec m t s) =
      -- the terminals of the opFam rule are themselves non terminals! So we
      -- need to fmap "simplifyPredicates"
      OperandSpec m (simplifyPredicates oracle (fmap (simplifyPredicates oracle) t)) s

   getTerminals (OperandSpec m ts s) =
      [ OperandSpec m t s | os <- getTerminals ts
                          , t  <- getTerminals os
                          ]

   getPredicates (OperandSpec _ t _) =
      nub (getPredicates t ++ concatMap getPredicates (getTerminals t))

-- | Operand access mode
data AccessMode
   = RO         -- ^ Read-only
   | RW         -- ^ Read-write
   | WO         -- ^ Write-only
   | NA         -- ^ Meta use of the operand
   deriving (Show,Eq)


-- | Is the operand encoding an immediate?
isImmediate :: OperandStorage -> Bool
isImmediate = \case
   S_Imm    -> True
   S_Imm8h  -> True
   S_Imm8l  -> True
   _        -> False

-- | Convert an operand family into an operand
opFamToOp :: OperandFamT -> Maybe Operand
opFamToOp op = case op of
   T_Reg r    -> OpReg <$> regFamToReg r
   T_Mem m    -> OpMem <$> x86memFamToMem m
   T_Imm i    -> OpImm <$> immFamToImm i
   T_Pair x y -> case (x,y) of
      -- we allow the first register of the pair to be not reducible
      -- to encode pair families such as AX, DX:AX, EDX:EAX
      (T_Reg r1,T_Reg r2) -> case regFamToReg r1 of
         Nothing  -> OpReg         <$> regFamToReg r2
         Just r1' -> OpRegPair r1' <$> regFamToReg r2
      (T_Imm i1,T_Imm i2) -> OpImmPair <$> immFamToImm i1
                                       <*> immFamToImm i2
      _ -> error ("opFamToOp: invalid pair: " ++ show (x,y))
