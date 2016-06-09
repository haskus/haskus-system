{-# LANGUAGE LambdaCase #-}

-- | X86 disassembler
module ViperVM.Arch.X86_64.Disassembler
   ( linearDisass
   , Disass (..)
   )
   where

import ViperVM.Format.Binary.Get as G
import ViperVM.Format.Binary.Buffer
import ViperVM.Arch.X86_64.ISA.Decoder
import ViperVM.Arch.X86_64.ISA.Insn

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
                                    else [RawBytes (offset - bufferSize fb - n) fb (reverse fbs), s]
                              s = Instruction offset (bufferTake n b) i
