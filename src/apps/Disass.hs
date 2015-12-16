
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
      g  = decodeMany (LongMode Long64bitMode) [] AddrSize64 OpSize32
      rs = G.runGetOrFail g bs

   putStrLn (show rs)
