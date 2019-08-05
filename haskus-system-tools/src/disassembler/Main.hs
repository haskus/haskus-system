{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import Control.Monad (forM_)

import Haskus.Binary.Buffer
import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.Insn
import Haskus.Arch.X86_64.Disassembler
import qualified Data.Set as Set

main :: IO ()
main = do
   [f] <- getArgs
   bs  <- bufferReadFile f

   let 
      m = ExecMode
            { x86Mode            = LongMode Long64bitMode
            , csDescriptorFlagD  = False
            , ssDescriptorFlagB  = False
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

   let
      ds = linearDisass m bs
      showDisass = \case
         RawBytes    offset buf errs -> showInsn offset buf ("; Failed: " ++ show errs)
         Instruction offset buf ins  -> showInsn offset buf d
            where
               d = insnMnemonic (insnSpec ins)
                    ++ " " ++ show (insnOperands ins)
                    ++ " " ++ show (Set.toList (insnVariant ins))


   forM_ ds showDisass

   putStrLn ""
   putStrLn "============================================="
   putStrLn "Show naive basic blocks"
   forM_ (findBlocks ds) $ \b -> do
      putStrLn "--------------------"
      putStrLn "BEGIN BLOCK"
      forM_ b showDisass
      putStrLn "END BLOCK"
      putStrLn "--------------------"
