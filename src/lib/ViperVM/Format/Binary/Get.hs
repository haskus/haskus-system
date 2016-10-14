{-# lANGUAGE LambdaCase #-}

-- | Get utilities
module ViperVM.Format.Binary.Get
   ( Get
   , runGet
   , runGetOrFail
   -- * Size & alignment
   , isEmpty
   , remaining
   , skip
   , uncheckedSkip
   , skipAlign
   , uncheckedSkipAlign
   , countBytes
   , alignAfter
   -- * Isolation
   , consumeExactly
   , consumeAtMost
   -- * Look-ahead
   , lookAhead
   , lookAheadM
   , lookAheadE
   -- * Read
   , getRemaining
   , getTextUtf8
   , getTextUtf8Nul
   , getBuffer
   , getBufferNul
   , getWord8
   , getWord16le
   , getWord16be
   , getWord32le
   , getWord32be
   , getWord64le
   , getWord64be
   -- * Utilities
   , getWhile
   , getWhole
   , getBitGet
   , getManyAtMost
   , getManyBounded
   )
where

import qualified Data.Serialize.Get as BG
import Data.Serialize.Get (Get)

import qualified ViperVM.Format.Text as Text
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Bits.Order
import ViperVM.Format.Binary.Bits.Get (BitGet, runBitGetPartial, skipBitsToAlignOnWord8M, bitGetStateInput)
import ViperVM.Utils.Maybe


-- | Test whether all input *in the current chunk* has been consumed
isEmpty :: Get Bool
isEmpty = BG.isEmpty

-- | Get the number of remaining unparsed bytes *in the current chunk*
remaining :: Get Word
remaining = fromIntegral <$> BG.remaining

-- | Skip ahead n bytes. Fails if fewer than n bytes are available.
skip :: Word -> Get ()
skip = BG.skip . fromIntegral

-- | Skip ahead n bytes. No error if there isn't enough bytes.
uncheckedSkip :: Word -> Get ()
uncheckedSkip = BG.uncheckedSkip . fromIntegral

-- | Skip to align n to al. Fails if fewer than n bytes are available.
skipAlign :: Word -> Word -> Get ()
skipAlign n al = skip n'
   where
      n' = case n `mod` al of
               0 -> 0
               x -> al - fromIntegral x

-- | Skip to align n to al. Fails if fewer than n bytes are available.
uncheckedSkipAlign :: Word -> Word -> Get ()
uncheckedSkipAlign n al = uncheckedSkip n'
   where
      n' = case n `mod` al of
               0 -> 0
               x -> al - fromIntegral x

-- | Run the getter without consuming its input. Fails if it fails
lookAhead :: Get a -> Get a
lookAhead = BG.lookAhead

-- | Run the getter. Consume its input if Just _ returned. Fails if it fails
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM = BG.lookAheadM

-- | Run the getter. Consume its input if Right _ returned. Fails if it fails
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE = BG.lookAheadE

-- | Require an action to consume exactly the given number of bytes, fail
-- otherwise
consumeExactly :: Word -> Get a -> Get a
consumeExactly sz = BG.isolate (fromIntegral sz)

-- | Require an action to consume at most the given number of bytes, fail
-- otherwise
consumeAtMost :: Word -> Get a -> Get a
consumeAtMost sz f = do
   sz' <- remaining
   (r,res) <- BG.lookAhead $ BG.isolate (fromIntegral (min sz sz')) $ do
      res <- f
      r <- remaining
      skip r -- skip remaining bytes, to make isolate happy
      return (r,res)
   skip (min sz' sz - r)
   return res

-- | Pull n bytes from the input, as a Buffer
getBuffer :: Word -> Get Buffer
getBuffer sz = Buffer <$> BG.getBytes (fromIntegral sz)

-- | Pull n bytes from the input, as a Buffer
getTextUtf8 :: Word -> Get Text.Text
getTextUtf8 sz = Text.bufferDecodeUtf8 <$> getBuffer sz

-- | Pull \0 terminal text
getTextUtf8Nul :: Get Text.Text
getTextUtf8Nul = Text.bufferDecodeUtf8 <$> getBufferNul

-- | Get Word8
getWord8 :: Get Word8
getWord8 = BG.getWord8

-- | Get Word16 little-endian
getWord16le :: Get Word16
getWord16le = BG.getWord16le

-- | Get Word16 big-endian
getWord16be :: Get Word16
getWord16be = BG.getWord16be

-- | Get Word32 little-endian
getWord32le :: Get Word32
getWord32le = BG.getWord32le

-- | Get Word32 big-endian
getWord32be :: Get Word32
getWord32be = BG.getWord32be

-- | Get Word64 little-endian
getWord64le :: Get Word64
getWord64le = BG.getWord64le

-- | Get Word64 big-endian
getWord64be :: Get Word64
getWord64be = BG.getWord64be

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
getRemaining :: Get Buffer
getRemaining = do
   r <- remaining
   getBuffer r


-- | Count the number of bytes consumed by a getter
countBytes :: Get a -> Get (Word, a)
countBytes g = do
   cnt0 <- remaining
   r <- g
   cnt1 <- remaining
   return (cnt0 - cnt1, r)

-- | Execute the getter and align on the given number of Word8
alignAfter :: Word -> Get a -> Get a
alignAfter alignment getter = do
   (cnt,r) <- countBytes getter
   uncheckedSkipAlign cnt alignment
   return r

-- | Get Buffer terminated with \0 (consume \0)
getBufferNul :: Get Buffer
getBufferNul = do
   bs <- lookAhead getRemaining
   let v = bufferTakeWhile (/= 0) bs
   uncheckedSkip (bufferSize v + 1)
   return v

-- | Run the Get monad
runGet :: Get a -> Buffer -> Either String a
runGet g (Buffer bs) = BG.runGet g bs

-- | Run a getter and throw an exception on error
runGetOrFail :: Get a -> Buffer -> a
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

-- | Apply the getter at most 'max' times
getManyAtMost :: Word -> Get (Maybe a) -> Get [a]
getManyAtMost mx f = fromMaybe [] <$> getManyBounded Nothing (Just mx) f

-- | Apply the getter at least 'min' times and at most 'max' times
getManyBounded :: Maybe Word -> Maybe Word -> Get (Maybe a) -> Get (Maybe [a])
getManyBounded _ (Just 0) _  = return (Just [])
getManyBounded (Just 0) mx f = getManyBounded Nothing mx f
getManyBounded mn mx f       = lookAheadM $ f >>= \case
      Nothing -> case mn of
         Just n | n > 0 -> return Nothing
         _              -> return (Just [])
      Just x -> fmap (x:) <$> getManyBounded (minus1 mn) (minus1 mx) f
   where
      minus1 = fmap (\k -> k - 1)

