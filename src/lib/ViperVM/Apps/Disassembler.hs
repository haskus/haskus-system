{-# LANGUAGE LambdaCase #-}

module ViperVM.Apps.Disassembler
   ( disassX86_64
   )
where


import qualified ViperVM.Format.Text as Text
import ViperVM.Format.Text (Text)
import ViperVM.Format.Binary.Buffer
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.X86_64.ISA.Mode
import ViperVM.Arch.X86_64.ISA.Size
import ViperVM.Arch.X86_64.ISA.Decoder
import ViperVM.Arch.X86_64.ISA.Insn
import ViperVM.Arch.X86_64.Disassembler

import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy as LT

-- | Disassemble a buffer containing X86-64 assembly.
-- Enable all the extensions
disassX86_64 :: Buffer -> Text
disassX86_64 buffer = LT.toStrict (toLazyText bld)
   where
      -- disassembled buffer
      ds = linearDisass m buffer


      -- arch mode
      m = ExecMode
            { x86Mode            = LongMode Long64bitMode
            , defaultAddressSize = AddrSize64
            , defaultOperandSize = OpSize32
            , extensions         = allExtensions
            }

      -- builder
      bld = mconcat (fmap (fromText . showDisass) ds)

      -- show a disassembled entry
      showDisass = \case
         RawBytes    offset buf errs -> showInsn offset buf ("; Failed: " ++ show errs)
         Instruction offset buf ins  -> showInsn offset buf d
            where
               d = insnMnemonic (insnSpec ins)
                    ++ " " ++ show (insnOperands ins)
                    ++ " " ++ show (BitSet.toList (insnVariant ins))

      -- show an instruction
      showInsn o b cmt = Text.pack str
         where
            o'     = show o
            b'     = show b
            fill c = replicate c ' '
            str = o'
                  ++ fill (10 - fromIntegral (length o'))
                  ++ b'
                  ++ fill (30 - fromIntegral (length b'))
                  ++ cmt

