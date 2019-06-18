{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.Disassembler
   ( disassX86_64
   )
where

import Prelude hiding (replicate,length)

import qualified Haskus.Utils.Text as Text
import Haskus.Utils.Text (Text)
import Haskus.Format.Binary.Buffer
import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Insn
import Haskus.Arch.X86_64.ISA.Encoding
import Haskus.Arch.X86_64.ISA.Register
import Haskus.Arch.X86_64.ISA.Immediate
import Haskus.Arch.X86_64.ISA.Memory
import Haskus.Arch.Common.Memory
import Haskus.Arch.X86_64.ISA.Operand
import Haskus.Arch.X86_64.Disassembler
import Haskus.Utils.List
import Haskus.Utils.Maybe

import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy as LT
import qualified Data.Set       as Set
import Numeric (showHex)

-- | Disassemble a buffer containing X86-64 assembly.
-- Enable all the extensions
disassX86_64 :: Maybe Word -> Buffer -> Text
disassX86_64 initOffset buffer = LT.toStrict (toLazyText bld)
   where
      -- disassembled buffer
      ds = linearDisass m buffer

      -- arch mode
      m = ExecMode
            { x86Mode            = LongMode Long64bitMode
            , csDescriptorFlagD  = False
            , ssDescriptorFlagB  = False
            , extensions         = allExtensions
            }

      -- builder
      bld = mconcat (fmap (fromText . showDisass) ds)


      -- show an instruction
      showInsn o b cmt = Text.pack str
         where
            o'     = showHex (fromMaybe 0 initOffset + o) ""
            b'     = show b
            fill c = replicate c ' '
            str = o'
                  ++ fill (17 - fromIntegral (length o'))
                  ++ b'
                  ++ fill (30 - fromIntegral (length b'))
                  ++ cmt
                  ++ "\n"

      -- show a disassembled entry
      showDisass = \case
         RawBytes    offset buf _   -> showInsn offset buf "; raw bytes"
         Instruction offset buf ins -> showInsn offset buf $
            (if not (Set.null (insnVariant ins))
               then show (Set.toList (insnVariant ins)) ++ " "
               else ""
            )
            ++ insnMnemonic (insnSpec ins)
            ++ " "
            ++ concat (intersperse ", " (fmap (uncurry showAsmOperand)
                  (insnOperands ins `zip` encOperands (insnEncoding ins))))

showAsmOperand :: Operand -> OperandSpec t -> String
showAsmOperand op enc = fimp $ case op of
   OpImm v         -> showAsmImm v
   OpMem m         -> showAsmMem m
   OpReg reg       -> showAsmReg reg
   OpRegPair r1 r2 -> showAsmReg r1 ++ ":" ++ showAsmReg r2
   OpImmPair i1 i2 -> showAsmImm i1 ++ ":" ++ showAsmImm i2
   where
      fimp x
         | opStore enc == S_Implicit = "{" ++ x ++ "}"
         | otherwise                 = x

-- TODO: show mem type
showAsmMem :: X86Mem -> String
showAsmMem m = cs ++ "[" ++ xs ++ "]"
   where
      a  = memAddr m
      cs = fromMaybe "" (fmap ((++":").showAsmReg) (addrSeg a))
      xs = concat (intersperse " + " (catMaybes [bs, is, ds]))
      bs = showAsmReg <$> addrBase a
      is = case (addrIndex a, addrScale a) of
         (Nothing, _)          -> Nothing
         (Just i, Just Scale1) -> Just (showAsmReg i)
         (Just i, Nothing)     -> Just (showAsmReg i)
         (Just i, Just Scale2) -> Just (showAsmReg i >> "*2")
         (Just i, Just Scale4) -> Just (showAsmReg i >> "*4")
         (Just i, Just Scale8) -> Just (showAsmReg i >> "*8")
      ds = (show . fromSizedValue) <$> addrDisp a

showAsmImm :: X86Imm -> String
showAsmImm = show . immValue

showAsmReg :: X86Reg -> String
showAsmReg reg = Text.unpack (registerName reg)
