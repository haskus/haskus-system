{-# LANGUAGE LambdaCase #-}

-- | X86 disassembler
module Haskus.Arch.X86_64.Disassembler
   ( Disass (..)
   , linearDisass
   , findBlocks
   )
   where

import Haskus.Format.Binary.Get as G
import Haskus.Format.Binary.Buffer
import Haskus.Arch.X86_64.ISA.Insn
import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.Decoder
import Haskus.Utils.List (intersect)

data Disass
   = RawBytes    Word Buffer [String]
   | Instruction Word Buffer Insn
   deriving (Show)

-- | Disassemble a whole buffer linearly
linearDisass :: ExecMode -> Buffer -> [Disass]
linearDisass m = go 0 emptyBuffer []

   where 
      g  = G.countBytes $ getInstruction m

      go offset fb fbs b
         | isBufferEmpty b && isBufferEmpty fb = []
         | isBufferEmpty b                     = [RawBytes (offset - bufferSize fb) fb fbs]

      go offset fb fbs b = case G.runGet g b of
            Left str    -> go (offset+1) (bufferSnoc fb (bufferHead b)) 
                              (reverse (str:fbs)) (bufferTail b)
            Right (n,i) -> x ++ go (offset + n) emptyBuffer [] (bufferDrop n b)
                           where 
                              x = if isBufferEmpty fb
                                    then [s]
                                    else [RawBytes (offset - bufferSize fb) fb (reverse fbs), s]
                              s = Instruction offset (bufferTake n b) i


-- | Find basic blocks by looking at branching/calls
-- Warning: we don't look at branch targets!
findBlocks :: [Disass] -> [[Disass]]
findBlocks = go []
   where
      go [] [] = []
      go bs [] = [reverse bs]
      go bs (d@RawBytes {}:ds) = go (d:bs) ds
      go bs (d@(Instruction _ _ i):ds) =
         if null (insnFamilies (insnSpec i)
            `intersect` [Call,Branch,ConditionalBranch,Return])
               then go (d:bs) ds
               else reverse (d:bs) : go [] ds
