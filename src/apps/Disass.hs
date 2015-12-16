
import System.Environment
import qualified Data.ByteString as BS
import ViperVM.Format.Binary.Get as G
import ViperVM.Arch.X86_64.Assembler.Decoder
import ViperVM.Arch.X86_64.Assembler.Mode
import ViperVM.Arch.X86_64.Assembler.OperandSize
import ViperVM.Arch.X86_64.Assembler.Size

main :: IO ()
main = do
   [f] <- getArgs
   bs <- BS.readFile f

   let 
      g  = G.countBytes $ decode (LongMode Long64bitMode) [] AddrSize64 OpSize32

      go b = do
         let (n,r) = G.runGetOrFail g b
         case r of
            Left x  -> putStrLn $ "Failed: " ++ show x
            Right x -> putStrLn (show x) >> putStrLn ""
         let b' = BS.drop n b
         if BS.null b'
            then return ()
            else go b'

   go bs
