{-# LANGUAGE ScopedTypeVariables #-}

-- | Terminal helpers
module ViperVM.System.Terminal
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

import ViperVM.System.Sys
import ViperVM.System.Process
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.Terminal (stdin,stdout)
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.FileSystem.ReadWrite (sysRead,sysWrite)
import ViperVM.Utils.STM.TList as TList
import ViperVM.Utils.STM.Future
import ViperVM.Utils.Memory
import ViperVM.Utils.Flow (void,when,forever)
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Text
import ViperVM.Format.String (withCStringLen)

import Control.Concurrent
import Control.Concurrent.STM
import System.Posix.Types (Fd(..))

-- | Terminal (input and output, no error output)
data Terminal = Terminal
   { termOut :: OutputState
   , termIn  :: InputState
   }


-- | Bufferized input
--
-- Read an input stream and copy the data:
--  * in the supplied requester buffer (zero-copy)
--  * in a buffer if there are no request pending
data InputState = InputState
   { inputRequests :: TList (IOBuffer, FutureSource ())
   , inputBuffer   :: TMVar InputBuffer
   , inputHandle   :: Handle
   }

-- | Input buffer
data InputBuffer = InputBuffer
   { inputBufferPtr   :: Ptr () -- ^ Buffer pointer
   , inputBufferSize  :: Word64 -- ^ Buffer size
   , inputBufferStart :: Word64 -- ^ Start offset of the input values
   , inputBufferStop  :: Word64 -- ^ End offset of the input values
   }

-- | Buffer
data IOBuffer = IOBuffer
   { iobufferSize :: Word64
   , iobufferPtr  :: Ptr ()
   }

inputThread :: InputState -> IO ()
inputThread s = forever $ do
   
   let hdl@(Handle fd) = inputHandle s

   threadWaitRead (Fd (fromIntegral fd))

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
                        let buf' = IOBuffer (size-size') (ptr `indexPtr` fromIntegral size')
                        TList.append_ (buf',semsrc) (inputRequests s)
            return (after,size,ptr)

         -- otherwise, use the remaining space in the input buffer
         Nothing -> do
            b <- takeTMVar (inputBuffer s)
            let
               size = inputBufferSize b - inputBufferStop b
               ptr  = inputBufferPtr b `indexPtr` fromIntegral (inputBufferStop b)
               after size' = do
                  let
                     b' = b { inputBufferStop = inputBufferStop b + size' }
                  putTMVar (inputBuffer s) b'

            -- if there is no room left, we retry
            if size == 0
               then retry
               else return (after,fromIntegral size,ptr)
                        
   readBytes <- runSys $ sysCallAssertQuiet ("Read bytes from "++show hdl) $ 
      sysRead hdl ptr sz

   -- TODO: if readBytes is zero, it's the end of file, etc.
   runSys' $ sysAssert "readBytes /= 0" (readBytes /= 0)

   atomically $ after readBytes


readFromHandle :: InputState -> Word64 -> Ptr () -> IO (Future ())
readFromHandle s sz ptr = do
   (after,bsz,bptr) <- atomically $ do
      -- read bytes from the buffer if any
      b <- takeTMVar (inputBuffer s)
      let 
         size   = inputBufferStop b - inputBufferStart b
         size'  = min (fromIntegral size) sz -- number of bytes taken from the buffer
         start' = inputBufferStart b + size'
         b'     = if start' == inputBufferStop b
                     -- if we read all the bytes, we reset start and stop
                     then InputBuffer 
                              { inputBufferPtr   = inputBufferPtr b
                              , inputBufferSize  = inputBufferSize b
                              , inputBufferStart = 0
                              , inputBufferStop  = 0
                              }
                     else b { inputBufferStart = start' }
         after  = putTMVar (inputBuffer s) b'
      return (after, size', inputBufferPtr b `indexPtr` fromIntegral (inputBufferStart b))

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
            let b = IOBuffer (sz - bsz) (ptr `indexPtr` fromIntegral bsz)
            TList.prepend_ (b,semsrc) (inputRequests s)
      return sem

      
            
-- | New buffered input with given buffer size
newInputState :: Word64 -> Handle -> IO InputState
newInputState size fd = do
   ptr <- mallocBytes (fromIntegral size)
   req <- atomically TList.empty
   mv  <- newTMVarIO (InputBuffer ptr size 0 0)
   return $ InputState req mv fd
      

data OutputState = OutputState
   { outputBuffers :: TList (IOBuffer, FutureSource ())
   , outputHandle  :: Handle
   }

outputThread :: OutputState -> IO ()
outputThread s = forever $ do
   let hdl@(Handle fd) = outputHandle s

   (buf,semsrc) <- atomically $ do
      e <- TList.last (outputBuffers s)
      case e of
         Nothing -> retry
         Just e' -> do
            TList.delete e' 
            return (TList.value e')

   threadWaitWrite (Fd (fromIntegral fd))

   -- try to write as much as possible
   n <- runSys $ sysCallAssertQuiet ("Write bytes to "++show hdl) $ 
      sysWrite hdl (iobufferPtr buf) (iobufferSize buf)

   atomically $ if n == iobufferSize buf
      then setFuture () semsrc
      else do
         let buf' = IOBuffer (iobufferSize buf - n)
                             (iobufferPtr buf `indexPtr` fromIntegral n)
                           
         TList.append_ (buf',semsrc) (outputBuffers s)
   
newOutputState :: Handle -> IO OutputState
newOutputState fd = do
   req <- atomically TList.empty
   return $ OutputState req fd

-- | Initialize a default terminal (using stdin, stdout)
defaultTerminal :: Sys Terminal
defaultTerminal = do
   -- switch to non-blocking modes
   let flgs = BitSet.fromList [ HandleNonBlocking
                              , HandleCloseOnExec
                              ]
   _ <- setHandleFlags stdin  flgs
   _ <- setHandleFlags stdout flgs

   -- TODO: set terminal buffering mode?

   -- input
   inState <- sysIO $ newInputState (16 * 1024) stdin
   sysFork "Terminal input handler"$ sysIO $ inputThread inState

   -- output
   outState <- sysIO $ newOutputState stdout
   sysFork "Terminal output handler" $ sysIO $ outputThread outState

   return $ Terminal outState inState

writeToHandle :: OutputState -> Word64 -> Ptr () -> IO (Future ())
writeToHandle s sz ptr = atomically $ do
   (sem,semsrc) <- newFuture
   TList.prepend_ (IOBuffer sz ptr, semsrc) (outputBuffers s)
   return sem

-- | Write bytes
writeTermBytes :: Terminal -> Word64 -> Ptr a -> IO (Future ())
writeTermBytes term sz ptr = writeToHandle (termOut term) sz (castPtr ptr)

-- | Write a string
writeStrLn :: Terminal -> String -> Sys ()
writeStrLn term s =
   sysIO $ withCStringLen s $ \(ptr,len) ->
      with '\n' $ \ptr2 -> do
         _   <- writeTermBytes term (fromIntegral len) (castPtr ptr)
         sem <- writeTermBytes term 1 (castPtr ptr2)
         atomically (waitFuture sem)

-- | Write a buffer
writeBuffer :: Terminal -> Buffer -> Sys ()
writeBuffer term b =
   sysIO $ bufferUnsafeUsePtr b $ \ptr len -> do
      sem <- writeTermBytes term (fromIntegral len) (castPtr ptr)
      atomically (waitFuture sem)

-- | Write a buffer
writeBufferLn :: Terminal -> Buffer -> Sys ()
writeBufferLn term b =
   sysIO $ bufferUnsafeUsePtr b $ \ptr len ->
      with '\n' $ \ptr2 -> do
         _   <- writeTermBytes term (fromIntegral len) (castPtr ptr)
         sem <- writeTermBytes term 1 (castPtr ptr2)
         atomically (waitFuture sem)

-- | Write a text using UTF8 encoding
writeText :: Terminal -> Text -> Sys ()
writeText term = writeBuffer term . textEncodeUtf8

-- | Write a text using UTF8 encoding
writeTextLn :: Terminal -> Text -> Sys ()
writeTextLn term = writeBufferLn term . textEncodeUtf8

-- | Read bytes (asynchronous)
readTermBytes :: Terminal -> Word64 -> Ptr a -> IO (Future ())
readTermBytes term sz ptr = readFromHandle (termIn term) sz (castPtr ptr)

-- | Read a Storable (synchronous)
readTerm :: Storable a => Terminal -> Sys a
readTerm term = sysIO $
   alloca $ \(ptr :: Ptr a) -> do
      sem <- readTermBytes term (fromIntegral $ sizeOf (undefined :: a)) ptr
      atomically $ waitFuture sem
      peek ptr

-- | Wait for a key to pressed
waitForKey :: Terminal -> Sys ()
waitForKey term = void (readTerm term :: Sys Word8)

