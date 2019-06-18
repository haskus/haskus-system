{-# LANGUAGE OverloadedStrings #-}

module Haskus.System.Process.MemoryMap
   ( module Haskus.System.Linux.Process.MemoryMap
   , showProcessMemoryMap
   )
where

import Haskus.System.Linux.Process.MemoryMap
import Haskus.System.Terminal
import Haskus.System.Sys
import Haskus.Utils.Text
import Haskus.Utils.Flow

showProcessMemoryMap :: Terminal -> [MemoryMapEntry] -> Sys ()
showProcessMemoryMap term x = do
   writeTextLn term (textFormat
      ( (center 25 ' ' %. text)
      % " "
      % text
      % " "
      % text
      )
      "Memory range"
      "Flgs"
      "Mapping"
      )

   let hasReadPerm [] = False
       hasReadPerm (PermRead:_) = True
       hasReadPerm (_:xs) = hasReadPerm xs

   let hasWritePerm [] = False
       hasWritePerm (PermWrite:_) = True
       hasWritePerm (_:xs) = hasWritePerm xs

   let hasExecPerm [] = False
       hasExecPerm (PermExec:_) = True
       hasExecPerm (_:xs) = hasExecPerm xs

   forM_ x <| \y -> do
      writeTextLn term (textFormat
         ((left 12 '0' %. hex)
         % "-"
         % (left 12 '0' %. hex)
         % " "
         % char % char % char % char
         % " "
         % stext
         )
         (entryStartAddr y)
         (entryStopAddr y)
         (if hasReadPerm (entryPerms y) then 'r' else '-')
         (if hasWritePerm (entryPerms y) then 'w' else '-')
         (if hasExecPerm (entryPerms y) then 'x' else '-')
         (case entrySharing y of
            Private -> 'p'
            Shared  -> 's'
         )
         (case entryType y of
            AnonymousMapping  -> ""
            NamedMapping s    -> textFormat ("[" % stext % "]") s
            fm@FileMapping {} -> textFormat (stext % " @ " % hex)
                                 (fileMappingPath fm)
                                 (fileMappingOffset fm)
         )
         )

