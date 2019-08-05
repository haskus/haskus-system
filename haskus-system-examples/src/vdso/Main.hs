{-# LANGUAGE OverloadedStrings #-}

import Haskus.System
import Haskus.Number.Word
import qualified Haskus.Utils.Text as Text
import Haskus.Utils.Text ((%),shown,textFormat)
import Haskus.Format.Elf
import Haskus.Format.Elf.Symbol
import Haskus.Utils.Maybe
import Haskus.Apps.Disassembler
import Foreign.Ptr

main :: IO ()
main = runSys' <| do

   sys  <- defaultSystemInit
   term <- defaultTerminal

   -- get the list of memory mappings for our own process
   maps <- getProcessMemoryMap sys
            |> catchEvalE (\xs -> sysError (textFormat ("Cannot retrieve memory map: " % shown) xs))

   let 
      isVDSO x = case entryType x of
                     NamedMapping t -> t == Text.pack "[vdso]" 
                     _              -> False

   -- look for the VDSO entry
   vdsoEntry <- case listToMaybe (filter isVDSO maps) of
      Just x  -> return x
      Nothing -> sysError "No vDSO memory map entry"

   -- read the ELF file
   vdso <- parseElf <$> liftIO (memoryMapToBuffer vdsoEntry)

   let 
      syms     = getAllSymbols vdso
      symTime  = snd . head <| filter ((== Just (Text.pack "__vdso_time")) . fst) syms
      timeAddr = symbolValue symTime + entryStartAddr vdsoEntry
      timePtr  = wordPtrToPtr (fromIntegral timeAddr)
      timeFun  = mkTime (castPtrToFunPtr timePtr)

   --print symbols
   writeStrLn term (show (fmap fst syms))
   writeStrLn term (show syms)

   -- disassemble some vDSO functions
   let ff (Just symName,_) = Text.pack "__vdso_" `Text.isPrefixOf` symName
       ff _                = False
   forM_ (filter ff syms) <| \(Just name,sym) -> do
      let symAddr = symbolValue sym + entryStartAddr vdsoEntry

      symAsm  <- disassX86_64 (Just (fromIntegral symAddr)) <$> liftIO (getSymbolBuffer sym (entryStartAddr vdsoEntry))
      writeText term name
      writeStrLn term ":"
      writeText term symAsm
      writeStrLn term ""

   -- test "time"
   r <- liftIO (timeFun nullPtr)
   writeStrLn term ("Time (UTC): Epoch + " ++ show r ++ " seconds")

   powerOff


type TimeFun = Ptr Word64 -> IO Word64
foreign import ccall unsafe "dynamic"
  mkTime :: FunPtr TimeFun -> TimeFun
