import Haskus.Utils.Flow
import Haskus.Arch.Linux.Process.MemoryMap
--import Haskus.Arch.Linux.Process.Auxiliary
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import Haskus.Format.Elf
--import Haskus.Format.Elf.Section
import Haskus.Format.Elf.Symbol

import qualified Haskus.Format.Text as Text

main :: IO ()
main = do

   -- get the list of memory mappings for our own process
   maps <- readMemoryMap "/proc/self/maps"

   let 
      isVDSO x = case entryType x of
                     NamedMapping t -> t == Text.pack "[vdso]" 
                     _              -> False

      -- look for the VDSO entry
      vdsoEntry = case filter isVDSO maps of
         [x] -> x
         []  -> error "No VDSO entry"
         _   -> error "Several VDSO entries"

   -- read the ELF file
   vdso <- parseElf <$> memoryMapToBuffer vdsoEntry

   let 
      syms     = getAllSymbols vdso
      symTime  = snd . head <| filter ((== Just (Text.pack "__vdso_time")) . fst) syms
      timeAddr = symbolValue symTime + entryStartAddr vdsoEntry
      timePtr  = wordPtrToPtr (fromIntegral timeAddr)
      timeFun  = mkTime (castPtrToFunPtr timePtr)

   print (fmap fst syms)

   r <- timeFun nullPtr
   putStrLn ("Time (UTC): Epoch + " ++ show r ++ " seconds")

   --TODO: print symbols (gettimeofday, etc.)
   -- TODO: disass
   
   -- TODO: test gettimeofday

   return ()


type TimeFun = Ptr Word64 -> IO Word64
foreign import ccall unsafe "dynamic"
  mkTime :: FunPtr TimeFun -> TimeFun
