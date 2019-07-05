{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Terminal helpers
module Haskus.System.Terminal
   ( Terminal
   , defaultTerminal
   , readTermBytes
   , readTerm
   , writeTermBytes
   , writeStrLn
   , writeBuffer
   , writeBufferLn
   , writeText
   , writeTextLn
   , waitForKey
   )
where

import Haskus.System.Sys
import Haskus.System.Process
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Error
import Haskus.System.Linux.Terminal (stdin,stdout)
import Haskus.System.Linux.FileSystem.ReadWrite
import Haskus.Utils.STM.TList as TList
import Haskus.Utils.STM.Future
import Haskus.Memory.Utils
import Haskus.Utils.Flow
import Haskus.Utils.STM
import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Storable
import Haskus.Utils.Text
import Haskus.Format.String (withCStringLen)

import Foreign.Ptr
import Foreign.Marshal.Alloc(mallocBytes)

-- | Terminal (input and output, no error output)
data Terminal = Terminal
   { termOut :: OutputState
   , termIn  :: InputState
   }

-- | Initialize a default terminal (using stdin, stdout)
defaultTerminal :: Sys Terminal
defaultTerminal = do
   -- switch to non-blocking modes
   let flgs = BitSet.fromList [ HandleNonBlocking
                              , HandleCloseOnExec
                              ]
   runE_ <| setHandleFlags stdin  flgs
   runE_ <| setHandleFlags stdout flgs

   -- TODO: set terminal buffering mode?

   -- input
   inState <- newInputState (16 * 1024) stdin
   sysFork "Terminal input handler" $ inputThread inState

   -- output
   outState <- newOutputState stdout
   sysFork "Terminal output handler" $ outputThread outState

   return $ Terminal outState inState


-- | Bufferized input
--
-- Read an input stream and copy the data:
--  * in the supplied requester buffer (zero-copy)
--  * in a ring buffer if there are no pending requests (buffer size is
--  configurable)
data InputState = InputState
   { inputRequests :: TList (IOBuffer, FutureSource ())
   , ringBuffer    :: TMVar RingBuffer
   , inputHandle   :: !Handle
   }

-- | Bufferized output
--
-- Write to an output stream when it is ready
--  * garanty ordering
data OutputState = OutputState
   { outputBuffers :: TList (IOBuffer, FutureSource ())
   , outputHandle  :: !Handle
   }

-- | Ring buffer
data RingBuffer = RingBuffer
   { ringBufferPtr   :: !(Ptr ()) -- ^ Buffer pointer
   , ringBufferSize  :: !Word64   -- ^ Buffer size
   , ringBufferStart :: !Word64   -- ^ Start offset of the input values
   , ringBufferStop  :: !Word64   -- ^ End offset of the input values
   }

-- | Buffer
data IOBuffer = IOBuffer
   { iobufferPtr  :: !(Ptr ())
   , iobufferSize :: !Word64
   }


inputThread :: InputState -> Sys ()
inputThread s = forever $ do
   
   let h = inputHandle s
   threadWaitRead h

   -- data are ready to be read
   (after, sz, ptr) <- atomically $ do
      e <- TList.last (inputRequests s)
      case e of
         -- if a request is pending, use its buffer
         Just e' -> do
            let
               buf         = fst (TList.value e')
               semsrc      = snd (TList.value e')
               size        = iobufferSize buf
               ptr         = iobufferPtr  buf
               after size' = do
                  TList.delete e'
                  if size' == size
                     -- the buffer is filled, we signal it
                     then setFuture () semsrc
                     -- we update the remaining number of bytes to read
                     else do
                        let buf' = IOBuffer (ptr `plusPtr` fromIntegral size')
                                            (size-size')
                        TList.append_ (buf',semsrc) (inputRequests s)
            return (after,size,ptr)

         -- otherwise, use the remaining space in the input buffer
         Nothing -> do
            b <- takeTMVar (ringBuffer s)
            let
               size = ringBufferSize b - ringBufferStop b
               ptr  = ringBufferPtr b `plusPtr` fromIntegral (ringBufferStop b)
               after size' = do
                  let
                     b' = b { ringBufferStop = ringBufferStop b + size' }
                  putTMVar (ringBuffer s) b'

            -- if there is no room left, we retry
            if size == 0
               then retry
               else return (after,fromIntegral size,ptr)
                        
   readBytes <- sysRead h ptr sz
                  |> assertLogShowErrorE (textFormat ("Read bytes from " % shown) h)

   -- TODO: if readBytes is zero, it's the end of file, etc.
   sysAssert "readBytes /= 0" (readBytes /= 0)

   atomically $ after readBytes


readFromHandle :: InputState -> Word64 -> Ptr () -> Sys (Future ())
readFromHandle s sz ptr = do
   (after,bsz,bptr) <- atomically $ do
      -- read bytes from the buffer if any
      b <- takeTMVar (ringBuffer s)
      let 
         size   = ringBufferStop b - ringBufferStart b
         size'  = min (fromIntegral size) sz -- number of bytes taken from the buffer
         start' = ringBufferStart b + size'
         b'     = if start' == ringBufferStop b
                     -- if we read all the bytes, we reset start and stop
                     then RingBuffer
                              { ringBufferPtr   = ringBufferPtr b
                              , ringBufferSize  = ringBufferSize b
                              , ringBufferStart = 0
                              , ringBufferStop  = 0
                              }
                     else b { ringBufferStart = start' }
         after  = putTMVar (ringBuffer s) b'
      return (after, size', ringBufferPtr b `plusPtr` fromIntegral (ringBufferStart b))

   when (bsz /= 0) $
      memCopy ptr bptr (fromIntegral bsz)

   atomically $ do
      -- put the buffer back
      after

      (sem,semsrc) <- newFuture

      if bsz == sz
         then setFuture () semsrc
         else do
            -- if we haven't read everything, register
            let b = IOBuffer (ptr `plusPtr` fromIntegral bsz) (sz - bsz)
            TList.prepend_ (b,semsrc) (inputRequests s)
      return sem

      
            
-- | New buffered input with given buffer size
newInputState :: MonadIO m => Word64 -> Handle -> m InputState
newInputState size fd = do
   ptr <- liftIO (mallocBytes (fromIntegral size))
   req <- atomically TList.empty
   mv  <- newTMVarIO (RingBuffer ptr size 0 0)
   return $ InputState req mv fd
      

outputThread :: OutputState -> Sys ()
outputThread s = go [] 0 0
   where
      h = outputHandle s

      -- writeMany handling EAGAIN
      wrt :: [(Ptr a, Word64)] -> Excepts '[ErrorCode] Sys Word64
      wrt ps = sysWriteMany h ps
                  |> catchLiftLeft \case
                        -- TODO: we should retry without having to rebuild the
                        -- parameter array (i.e. do it in sysWriteMany)
                        EAGAIN -> threadWaitWrite h >> wrt ps
                        err    -> failureE err


      go :: [(IOBuffer, FutureSource ())] -> Word -> Word -> Sys ()
      go bufs nbufs off = do
         -- wait for the handle to be ready
         threadWaitWrite h

         -- take as many output buffers as we can from the queue
         -- (Linux imposes a limit: maxIOVec)
         bufs' <- atomically $ do
            TList.take (maxIOVec - nbufs) (outputBuffers s) >>= \case
               -- block if there is no pending buffer to write
               [] | nbufs == 0 -> retry
               xs              -> return xs

         let
            -- total number of buffers
            ntot = nbufs + fromIntegral (Prelude.length bufs')
            -- all buffers
            bs = bufs ++ bufs'
            -- build a list of [(Ptr, Size)] from the buffers. The first buffer
            -- uses the offset "off", the others are fully considered.
            ps = case fmap fst bs of
               []                   -> []
               (IOBuffer p sz : xs) -> (p `plusPtr` fromIntegral off, sz - fromIntegral off) : fmap f xs
                  where
                     f (IOBuffer ptr siz) = (ptr,siz)

         -- write the buffers
         size <- wrt ps
                  |> assertShowE (textFormat ("Write bytes to " % shown) h)

         let
            sig xs nb 0 = go xs nb 0
            sig [] _ n = error ("Write: too many bytes written!? (" ++ show n ++ ")")
            sig ((IOBuffer _ sz, fut) : xs) nb n
               | n >= sz = do
                  -- signal that the buffer has been written
                  atomically $ setFuture () fut
                  -- continue with the next buffer
                  sig xs (nb-1) (n-sz)
            sig xs nb n = go xs nb (fromIntegral n)

         -- signal the written buffers
         sig bs ntot size


newOutputState :: MonadIO m => Handle -> m OutputState
newOutputState fd = do
   req <- atomically TList.empty
   return $ OutputState req fd

writeToHandle :: OutputState -> Word64 -> Ptr () -> STM (Future ())
writeToHandle s sz ptr = do
   (sem,semsrc) <- newFuture
   TList.append_ (IOBuffer ptr sz, semsrc) (outputBuffers s)
   return sem

-- | Write bytes
writeTermBytes :: Terminal -> Word64 -> Ptr a -> STM (Future ())
writeTermBytes term sz ptr = writeToHandle (termOut term) sz (castPtr ptr)

-- | Write a string
writeStrLn :: Terminal -> String -> Sys ()
writeStrLn term s =
   withCStringLen s $ \ptr len ->
      with '\n' $ \ptr2 -> do
         sem <- atomically $ do
            _   <- writeTermBytes term (fromIntegral len) (castPtr ptr)
            writeTermBytes term 1 (castPtr ptr2)
         atomically (waitFuture sem)

-- | Write a buffer
writeBuffer :: Terminal -> Buffer -> Sys ()
writeBuffer term b =
   bufferUnsafeUsePtr b $ \ptr len -> do
      sem <- atomically $
         writeTermBytes term (fromIntegral len) (castPtr ptr)
      atomically $ waitFuture sem

-- | Write a buffer
writeBufferLn :: Terminal -> Buffer -> Sys ()
writeBufferLn term b =
   bufferUnsafeUsePtr b $ \ptr len ->
      with '\n' $ \ptr2 -> do
         sem <- atomically $ do
            _   <- writeTermBytes term (fromIntegral len) (castPtr ptr)
            writeTermBytes term 1 (castPtr ptr2)
         atomically $ waitFuture sem

-- | Write a text using UTF8 encoding
writeText :: Terminal -> Text -> Sys ()
writeText term = writeBuffer term . textEncodeUtf8

-- | Write a text using UTF8 encoding
writeTextLn :: Terminal -> Text -> Sys ()
writeTextLn term = writeBufferLn term . textEncodeUtf8

-- | Read bytes (asynchronous)
readTermBytes :: Terminal -> Word64 -> Ptr a -> Sys (Future ())
readTermBytes term sz ptr = readFromHandle (termIn term) sz (castPtr ptr)

-- | Read a Storable (synchronous)
readTerm :: Storable a => Terminal -> Sys a
readTerm term =
   alloca $ \(ptr :: Ptr a) -> do
      sem <- readTermBytes term (sizeOfT' @a) ptr
      atomically $ waitFuture sem
      peek ptr

-- | Wait for a key to pressed
waitForKey :: Terminal -> Sys ()
waitForKey term = void (readTerm term :: Sys Word8)

