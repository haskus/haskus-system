{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Haskus.Arch.X86_64.ISA.Example
  ( example
  )
where

import Haskus.Arch.X86_64.ISA.Insn.ADC
import Haskus.Arch.X86_64.ISA.Ops
import qualified Control.Monad.State as S
import Data.Bifunctor
import System.IO.Extra
import System.Process
import Foreign.Marshal.Array
import Control.Monad

newtype CodeGen a = CodeGen (S.State CGState a)
  deriving newtype (Functor,Applicative,Monad)

data CGState = CGState
  { cg_bytes   :: ![Word8]
  , cg_pos     :: !Location
  }
  deriving (Show)

initCGState :: CGState
initCGState = CGState [] 0


instance Output CodeGen where
  putW8 b    = CodeGen $ S.modify \s -> s
                  { cg_bytes = b : cg_bytes s
                  , cg_pos   = cg_pos s + 1
                  }
  getLoc = CodeGen (S.gets cg_pos)

runCodeGen :: CodeGen a -> (a, CGState)
runCodeGen (CodeGen m) = second fix_state $ S.runState m initCGState
  where
    fix_state s = s
      { cg_bytes  = reverse (cg_bytes s)
      }

example :: IO ()
example = do

  let (r, state) = runCodeGen $ do
        let defOpSize = DefaultOperandSize32
        let rev_bit   = ReverseBit0
        r1 <- put $ ADC_AL_i8 0x15
        r2 <- put $ ADC_AL_i8 0x27
        r3 <- put $ ADC_AX_i16 defOpSize 0x0102
        put $ ADC_r32_r32 defOpSize RAX RBX rev_bit
        put $ ADC_r64_r64 RAX RBX rev_bit
        put $ ADC_r64_r64 RAX RBX ReverseBit1
        pure (r1,r2,r3)

  print r
  void $ withTempFile \fp -> do
    -- dump machine code into temporary file
    withBinaryFile fp WriteMode \hdl ->
      withArrayLen (cg_bytes state) \len ptr ->
        hPutBuf hdl ptr len

    -- call ndisasm on it in 64-bits mode
    rawSystem "ndisasm" ["-b", "64", fp]
