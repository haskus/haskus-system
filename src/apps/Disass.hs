{-# LANGUAGE LambdaCase #-}

import System.Environment
import Control.Monad (unless)

import ViperVM.Format.Binary.Get as G
import ViperVM.Format.Binary.Buffer
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.Size
import ViperVM.Arch.X86_64.Assembler.New
import ViperVM.Arch.X86_64.Assembler.Insns

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
      g  = G.countBytes $ getInstruction m

      go offset b = case G.runGet g b of
            Left str    -> do
               putStrLn $ "Failed: " ++ show str
               print b
            Right (n,i) -> do
               let
                  str = show offset
                        ++ "\t"
                        ++ show (bufferTake n b)
                        ++ replicate (30 - 2*fromIntegral n) ' '
                        ++ insnMnemonic (insnSpec i)
                        ++ " " ++ show (insnOperands i)
                        ++ " " ++ show (BitSet.toList (insnVariant i))
               putStrLn str
               let b' = bufferDrop n b
               unless (isBufferEmpty b') $
                  go (offset + n) b'
   go 0 bs
