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
  , cg_begin   :: !Location
  }
  deriving (Show)

initCGState :: CGState
initCGState = CGState
  { cg_bytes = []
  , cg_pos   = 0
  , cg_begin = 0
  }


instance Output CodeGen where
  putW8 b    = CodeGen $ S.modify \s -> s
                  { cg_bytes = b : cg_bytes s
                  , cg_pos   = cg_pos s + 1
                  }
  getLoc     = CodeGen (S.gets cg_pos)
  getInsnSize = CodeGen $ do
    orig <- S.gets cg_begin
    end  <- S.gets cg_pos
    pure (fromIntegral (end - orig))
  beginInsn  = CodeGen (S.modify \s -> s { cg_begin = cg_pos s })



runCodeGen :: CodeGen a -> (a, CGState)
runCodeGen (CodeGen m) = second fix_state $ S.runState m initCGState
  where
    fix_state s = s
      { cg_bytes  = reverse (cg_bytes s)
      }

data Labeled l a
  = Labeled l a
  deriving (Show,Eq,Ord)

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
        let addr = Addr (RIPRelative (Disp32 0x7700)) (Just CS) Nothing
        r4 <- put $ ADC_m32_i32 defOpSize NoLock addr 0x12345678
        put $ ADC_r64_r64 RAX RBX rev_bit
        pure ( Labeled "Some_reloc" r1
             , Labeled "some_other_reloc" r2
             , Labeled "and_another_one" r3
             , r4
             )

  print r
  void $ withTempFile \fp -> do
    -- dump machine code into temporary file
    withBinaryFile fp WriteMode \hdl ->
      withArrayLen (cg_bytes state) \len ptr ->
        hPutBuf hdl ptr len

    -- call ndisasm on it in 64-bits mode
    rawSystem "ndisasm" ["-b", "64", fp]
