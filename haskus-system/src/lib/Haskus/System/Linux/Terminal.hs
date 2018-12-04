{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides some functions to use Linux
-- terminals
module Haskus.System.Linux.Terminal
   ( stdin
   , stdout
   , stderr
   , writeStr
   , writeStrLn
   , readChar
   )
   where

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.FileSystem.ReadWrite
import Haskus.System.Linux.Handle
import Haskus.Utils.Flow
import Haskus.Format.Text
import Haskus.Format.String
import Haskus.Format.Binary.Buffer

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
writeStr :: MonadInIO m => Handle -> String -> FlowT '[ErrorCode] m ()
writeStr fd = writeBuffer fd . stringEncodeUtf8

-- | Write a String with a newline character in the given
-- file descriptor
writeStrLn :: MonadInIO m => Handle -> String -> FlowT '[ErrorCode] m ()
writeStrLn fd = writeBuffer fd . stringEncodeUtf8 . (++ "\n")

-- | Read a single character
--
-- Warning: only the first byte of multi-byte characters (e.g. utf8) will be
-- read
readChar :: MonadInIO m => Handle -> FlowT ReadErrors' m Char
readChar fd = handleReadBuffer fd Nothing 1
   ||> (castCCharToChar . bufferPeekStorable)
