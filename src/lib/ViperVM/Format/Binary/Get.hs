-- | Get utilities
module ViperVM.Format.Binary.Get
   ( module Data.Serialize.Get
   , getWhile
   , getWhole
   , getRemaining
   , getBitGet
   , runGetOrFail
   , getByteStringNul
   , countBytes
   , alignAfter
   )
where

import Data.Serialize.Get
import qualified Data.ByteString as BS
import Control.Monad (when)

import ViperVM.Format.Binary.BitOrder
import ViperVM.Format.Binary.BitGet (BitGet, runBitGetPartial, skipBitsToAlignOnWord8M, bitGetStateInput)

-- | Get while True (read and discard the ending element)
getWhile :: (a -> Bool) -> Get a -> Get [a]
getWhile cond getter = rec []
   where
      rec xs = do
         x <- getter
         if cond x
            then rec (x:xs)
            else return (reverse xs)

-- | Repeat the getter to read the whole bytestring
getWhole :: Get a -> Get [a]
getWhole getter = rec []
   where
      rec xs = do
         cond <- isEmpty
         if cond
            then return (reverse xs)
            else do
               x <- getter
               rec (x:xs)

-- | Get remaining bytes
getRemaining :: Get BS.ByteString
getRemaining = do
   r <- remaining
   getBytes r


-- | Count the number of bytes consumed by a getter
countBytes :: Get a -> Get (Int, a)
countBytes g = do
   cnt0 <- remaining
   r <- g
   cnt1 <- remaining
   return (cnt0 - cnt1, r)

-- | Execute the getter and align on the given number of Word8
alignAfter :: Int -> Get a -> Get a
alignAfter alignment getter = do
   (cnt,r) <- countBytes getter
   let toSkip = alignment - (cnt `mod` alignment)
   empty <- isEmpty
   when (toSkip /= alignment && toSkip /= 0 && not empty) $
      uncheckedSkip toSkip
   return r

-- | Get ByteString terminated with \0 (consume \0)
getByteStringNul :: Get BS.ByteString
getByteStringNul = do
   bs <- lookAhead getRemaining
   let v = BS.takeWhile (/= 0) bs
   uncheckedSkip (BS.length v + 1)
   return v

-- | Run a getter and throw an exception on error
runGetOrFail :: Get a -> BS.ByteString -> a
runGetOrFail g bs = case runGet g bs of
   Left err -> error err
   Right x  -> x


-- | Get bits from a BitGet. 
--
-- Discard last bits to align on a Word8 boundary
--
-- FIXME: we use a continuation because Data.Serialize.Get doesn't export "put"
getBitGet :: BitOrder -> BitGet a -> (a -> Get b) -> Get b
getBitGet bo bg cont = do
   bs <- getRemaining
   let (v,s) = runBitGetPartial bo (bg <* skipBitsToAlignOnWord8M) bs
   return $ runGetOrFail (cont v) (bitGetStateInput s)
