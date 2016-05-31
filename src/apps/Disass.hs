{-# LANGUAGE LambdaCase #-}

import System.Environment
import ViperVM.Format.Binary.Get as G
import ViperVM.Format.Binary.Buffer
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
            , extensions         =
               [
               ]
            }
      g  = G.countBytes $ getInstruction m

      go offset b = do
         case G.runGet g b of
            Left str    -> putStrLn $ "Failed: " ++ show str
            Right (n,(oc,ops,enc,insn)) -> do
               let
                  str = show offset
                        ++ "\t"
                        ++ show (bufferTake n b)
                        ++ replicate (30 - 2*fromIntegral n) ' '
                        ++ insnMnemonic insn
                        ++ " " ++ show ops
                        ++ " " ++ show n
               putStrLn str
               let b' = bufferDrop n b
               if isBufferEmpty b'
                  then return ()
                  else go (offset + n) b'
   go 0 bs
