{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System
import Haskus.Number.Word
import Haskus.Utils.Text (Text,(%),shown,textFormat)
import Haskus.Format.Elf
import Haskus.Format.Elf.Symbol
import Haskus.Utils.Maybe
import Haskus.Apps.Disassembler
import Foreign.Ptr


main :: IO ()
main = runSys' <| do

   sys  <- defaultSystemInit
   term <- defaultTerminal

   (vdsoEntry,vdso)  <- getVDSO sys
   (timeFun,timeAsm) <- getFun vdsoEntry vdso "__vdso_time" mkTime

   r <- liftIO (timeFun nullPtr)
   writeTextLn term (epochToText r)
   writeTextLn term "Assembly code:"
   writeText term timeAsm
   powerOff

-- | Test if a memory map entry is named "[vdso]"
isVDSO :: MemoryMapEntry -> Bool
isVDSO x = case entryType x of
   NamedMapping t -> t == "[vdso]"
   _              -> False

-- | Get the VDSO memory map entry and the corresponding ELF object
getVDSO :: System -> Sys (MemoryMapEntry, Elf)
getVDSO sys = do
   -- get the list of memory mappings for our own process
   maps <- getProcessMemoryMap sys
            |> catchEvalE \xs ->
                 sysError (textFormat ("Cannot retrieve memory map: " % shown) xs)

   -- look for the VDSO entry
   vdsoEntry <- case listToMaybe (filter isVDSO maps) of
      Just x  -> return x
      Nothing -> sysError "No vDSO memory map entry"

   -- parse the ELF format
   vdso <- parseElf <$> liftIO (memoryMapToBuffer vdsoEntry)

   return (vdsoEntry, vdso)


-- | Get a VDSO function
getFun :: MemoryMapEntry -> Elf -> Text -> (FunPtr a -> a) -> Sys (a,Text)
getFun vdsoEntry vdso name mkFun = do
   let
      syms    = getAllSymbols vdso
      funSym  = snd . head <| filter ((== Just name) . fst) syms
      funAddr = symbolValue funSym + entryStartAddr vdsoEntry
      funPtr  = wordPtrToPtr (fromIntegral funAddr)
      fun      = mkFun (castPtrToFunPtr funPtr)

   buf <- liftIO (getSymbolBuffer funSym (entryStartAddr vdsoEntry))
   let funAsm = disassX86_64 (Just (fromIntegral funAddr)) buf

   return (fun,funAsm)


-- Function to convert foreign "time" function into a Haskell function
foreign import ccall unsafe "dynamic"
  mkTime :: FunPtr TimeFun -> TimeFun

type TimeFun = Ptr Word64 -> IO Word64


-- | Pretty-print an EPOCH time
epochToText :: Word64 -> Text
epochToText n = textFormat fmt d hrs mins secs
   where
      fmt = shown % " days, " % shown % " hours, "
            % shown % " minutes, " % shown % " seconds since 1970-01-01 00:00:00"
      (m,secs) = quotRem n 60
      (h,mins) = quotRem m 60
      (d,hrs)  = quotRem h 24
