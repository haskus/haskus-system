{-# LANGUAGE DeriveAnyClass #-}

module Haskus.Arch.X86_64.ISA.Insn
   ( EncodingVariant(..)
   , Insn (..)
   , X86Insn(..)
   , Properties (..)
   , FlagOp(..)
   , Flag (..)
   , InsnFamily (..)
   )
where

import Haskus.Arch.X86_64.ISA.Encoding
import Haskus.Arch.X86_64.ISA.Operand
import Data.Set (Set)

-- | A fully defined instruction (isomorphic to its binary representation)
data Insn = Insn
   { insnOpcode   :: Opcode
   , insnOperands :: [Operand]
   , insnEncoding :: Encoding
   , insnSpec     :: X86Insn
   , insnVariant  :: Set EncodingVariant
   }
   deriving (Show)

-- | Instruction variant encoding
data EncodingVariant
   = Locked                     -- ^ Locked memory access
   | Reversed                   -- ^ Parameters are reversed (useful when some instructions have two valid encodings, e.g. CMP reg8, reg8)
   | ExplicitParam              -- ^ A variant exists with an implicit parameter, but the explicit variant is used
   | RepeatZero                 -- ^ REP(Z) prefix
   | RepeatNonZero              -- ^ REPNZ prefix
   | LockElisionAcquire         -- ^ XACQUIRE prefix
   | LockElisionRelease         -- ^ XRELEASE prefix
   | BranchHintTaken            -- ^ Branch hint (branch taken)
   | BranchHintNotTaken         -- ^ Branch hint (not taken)
   | SuperfluousSegmentOverride -- ^ Segment override equal to default segment
   | UselessRex Rex             -- ^ Additional useless Rex prefix
   -- TODO: legacy prefix order, superfluous prefixes
   deriving (Show,Eq,Ord)


-- | X86 instruction
data X86Insn = X86Insn
   { insnDesc        :: String
   , insnMnemonic    :: String
   , insnProperties  :: [Properties]
   , insnFamilies    :: [InsnFamily]
   , insnFlags       :: [FlagOp Flag]
   , insnEncodings   :: [Encoding]
   } deriving (Show)

-- | Instruction properties
data Properties
   = FailOnZero Int           -- ^ Fail if the n-th parameter (indexed from 0) is 0
   | MemAlign Int             -- ^ Memory alignment constraint in bytes
   | MemAlignDefault          -- ^ Memory alignment constraint
   deriving (Show,Eq)

-- | Instruction taxonomy
data InsnFamily
   = Call              -- ^ Call-like instruction (branch then return to the next instruction)
   | Return            -- ^ Return from a call, an interruption, etc.
   | Branch            -- ^ Unconditional branch instruction
   | ConditionalBranch -- ^ Conditional branch instruction
   deriving (Show,Eq)

-- | Flag state modification
data FlagOp a
   = St        [a]  -- ^ Set flag to 1
   | Unset     [a]  -- ^ Set flag to 0
   | Modified  [a]  -- ^ Set flag depending on the result
   | Undefined [a]  -- ^ Flag is undefined after the operation
   | Read      [a]  -- ^ Flag read by the instruction
   deriving (Show,Eq)

-- | Status flag
data Flag
   -- Status flag
   = CF     -- ^ Carry flag
   | PF     -- ^ Parity flag
   | AF     -- ^ Adjust flag
   | ZF     -- ^ Zero flag
   | SF     -- ^ Sign flag
   | TF     -- ^ Trap flag
   | OF     -- ^ Overflow flag

   -- Control flags
   | DF     -- ^ Direction flag
   | IF     -- ^ Interrupt flag
   | AC     -- ^ Alignment check
   deriving (Show,Enum,Eq)

