{-# LANGUAGE LambdaCase #-}

import System.Environment
import Control.Monad (forM_)

import ViperVM.Format.Binary.Buffer
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.X86_64.ISA.Mode
import ViperVM.Arch.X86_64.ISA.Size
import ViperVM.Arch.X86_64.ISA.Decoder
import ViperVM.Arch.X86_64.ISA.Insn
import ViperVM.Arch.X86_64.Disassembler

main :: IO ()
main = do
   [f] <- getArgs
   bs  <- bufferReadFile f

   let 
      m = ExecMode
            { x86Mode            = LongMode Long64bitMode
            , defaultAddressSize = AddrSize64
            , defaultOperandSize = OpSize32
            , extensions         = allExtensions
            }

      showInsn o b cmt = putStrLn str
         where
            o'     = show o
            b'     = show b
            fill c = replicate c ' '
            str = o'
                  ++ fill (10 - fromIntegral (length o'))
                  ++ b'
                  ++ fill (30 - fromIntegral (length b'))
                  ++ cmt


   forM_ (linearDisass m bs) $ \case
      Failure offset buf errs -> showInsn offset buf ("; Failed: " ++ show errs)
      Success offset buf ins  -> showInsn offset buf d
         where
            d = insnMnemonic (insnSpec ins)
                 ++ " " ++ show (insnOperands ins)
                 ++ " " ++ show (BitSet.toList (insnVariant ins))
