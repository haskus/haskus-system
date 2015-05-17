-- | This module provides some functions to use Linux
-- terminals
module ViperVM.Arch.Linux.Terminal
   ( stdin
   , stdout
   , stderr
   , writeStr
   , writeStrLn
   , readChar
   -- * Error handling
   , sysCatch
   , sysCatchFail
   , runCatch
   )
   where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.FileDescriptor

import Control.Monad.Trans.Either

import qualified Data.ByteString.Char8 as BS
import Data.Word

-- | Standard input (by convention)
stdin :: FileDescriptor
stdin = FileDescriptor 0

-- | Standard output (by convention)
stdout :: FileDescriptor
stdout = FileDescriptor 1

-- | Standard error output (by convention)
stderr :: FileDescriptor
stderr = FileDescriptor 2

-- | Write a String in the given file descriptor
writeStr :: FileDescriptor -> String -> SysRet Word64
writeStr fd = writeByteString fd . BS.pack

-- | Write a String with a newline character in the given
-- file descriptor
writeStrLn :: FileDescriptor -> String -> SysRet Word64
writeStrLn fd = writeByteString fd . BS.pack . (++ "\n")

-- | Read a single character
--
-- Warning: only the first byte of multi-byte characters (e.g. utf8) will be
-- read
readChar :: FileDescriptor -> SysRet Char
readChar fd = fmap (head . BS.unpack) <$> readByteString fd 1

sysCatch :: Show a => Either (String,a) b -> IO (Either a b)
sysCatch a = case a of
   Left (str,err) -> do
      writeStrLn stdout $ "Error while trying to " ++ str ++ " (" ++ show err ++ ")"
      return (Left err)
   Right r -> return (Right r)

sysCatchFail :: Show a => Either (String,a) b -> IO b
sysCatchFail a = do
   r <- sysCatch a
   case r of
      Left _  -> error "Error catch"
      Right b -> return b

runCatch :: Show a => EitherT (String,a) IO b -> IO (Either a b)
runCatch x = do
   ret <- runEitherT x
   sysCatch ret

