-- | This module provides some functions to use Linux
-- terminals
module ViperVM.Arch.Linux.Terminal
   ( stdin
   , stdout
   , stderr
   , writeStr
   , writeStrLn
   , readChar
   )
   where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.Handle
import ViperVM.Utils.Flow
import ViperVM.Format.Text
import ViperVM.Format.String
import ViperVM.Format.Binary.Buffer

-- | Standard input (by convention)
stdin :: Handle
stdin = Handle 0

-- | Standard output (by convention)
stdout :: Handle
stdout = Handle 1

-- | Standard error output (by convention)
stderr :: Handle
stderr = Handle 2

-- | Write a String in the given file descriptor
writeStr :: Handle -> String -> SysRet ()
writeStr fd = writeBuffer fd . stringEncodeUtf8

-- | Write a String with a newline character in the given
-- file descriptor
writeStrLn :: Handle -> String -> SysRet ()
writeStrLn fd = writeBuffer fd . stringEncodeUtf8 . (++ "\n")

-- | Read a single character
--
-- Warning: only the first byte of multi-byte characters (e.g. utf8) will be
-- read
readChar :: Handle -> SysRet Char
readChar fd = readBuffer fd 1
   >.-.> (castCCharToChar . bufferPeekStorable)
