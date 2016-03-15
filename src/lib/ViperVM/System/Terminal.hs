{-# LANGUAGE ScopedTypeVariables #-}

module ViperVM.System.Terminal
   ( Terminal
   , defaultTerminal
   , readTermBytes
   , writeTermBytes
   , writeStrLn
   , waitForKey
   )
where

import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Terminal (stdin,stdout)
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.FileSystem.ReadWrite (sysRead,sysWrite)
import ViperVM.Utils.STM.TList as TList
import ViperVM.Utils.STM.TNotify
import ViperVM.Utils.Memory
import ViperVM.Format.Binary.BitSet as BitSet

import Data.Word
import Control.Monad (void,when,forever)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import Control.Concurrent.STM
import System.Posix.Types (Fd(..))
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String (withCStringLen)
import Foreign.Marshal.Alloc


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
   { inputRequests :: TList (Buffer, TNotifySource ())
   , inputBuffer   :: TMVar InputBuffer
   , inputHandle   :: FileDescriptor
   }

data InputBuffer = InputBuffer
   { inputBufferPtr   :: Ptr () -- ^ Buffer pointer
   , inputBufferSize  :: Word64 -- ^ Buffer size
   , inputBufferStart :: Word64 -- ^ Start offset of the input values
   , inputBufferStop  :: Word64 -- ^ End offset of the input values
   }

data Buffer = Buffer
   { bufferSize :: Word64
   , bufferPtr  :: Ptr ()
   }

inputThread :: InputState -> IO ()
inputThread s = forever $ do
   
   let hdl@(FileDescriptor fd) = inputHandle s

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
               size        = bufferSize buf
               ptr         = bufferPtr  buf
               after size' = do
                  TList.delete e'
                  if size' == size
                     -- the buffer is filled, we signal it
                     then signalNotify () semsrc
                     -- we update the remaining number of bytes to read
                     else do
                        let buf' = Buffer (size-size') (ptr `plusPtr` fromIntegral size')
                        TList.append_ (buf',semsrc) (inputRequests s)
            return (after,size,ptr)

         -- otherwise, use the remaining space in the input buffer
         Nothing -> do
            b <- takeTMVar (inputBuffer s)
            let
               size = inputBufferSize b - inputBufferStop b
               ptr  = inputBufferPtr b `plusPtr` fromIntegral (inputBufferStop b)
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


readFromHandle :: InputState -> Word64 -> Ptr () -> IO (TNotify ())
readFromHandle s sz ptr = do
   (after,bsz,bptr) <- atomically $ do
      -- read bytes from the buffer if any
      b <- takeTMVar (inputBuffer s)
      let 
         size   = inputBufferSize b - inputBufferStop b
         size'  = min (fromIntegral size) sz -- number of bytes taken from the buffer
         start' = inputBufferStart b + size'
         b'     = if start' == inputBufferStop b
                     -- if we read all the bytes, we reset start and stop
                     then InputBuffer (inputBufferPtr b) (inputBufferSize b) 0 0
                     else b { inputBufferStart = start' }
         after  = putTMVar (inputBuffer s) b'
      return (after, size', inputBufferPtr b `plusPtr` fromIntegral (inputBufferStart b))

   when (bsz /= 0) $
      memCopy ptr bptr (fromIntegral bsz)

   atomically $ do
      -- put the buffer back
      after

      (sem,semsrc) <- newTNotify

      if bsz == sz
         then signalNotify () semsrc
         else do
            -- if we haven't read everything, register
            let b = Buffer (sz - bsz) (ptr `plusPtr` fromIntegral bsz)
            TList.prepend_ (b,semsrc) (inputRequests s)
      return sem

      
            
-- | New buffered input with given buffer size
newInputState :: Word64 -> FileDescriptor -> IO InputState
newInputState size fd = do
   ptr <- mallocBytes (fromIntegral size)
   req <- atomically TList.empty
   mv  <- newTMVarIO (InputBuffer ptr size 0 0)
   return $ InputState req mv fd
      

data OutputState = OutputState
   { outputBuffers :: TList (Buffer, TNotifySource ())
   , outputHandle  :: FileDescriptor
   }

outputThread :: OutputState -> IO ()
outputThread s = forever $ do
   let hdl@(FileDescriptor fd) = outputHandle s

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
      sysWrite hdl (bufferPtr buf) (bufferSize buf)

   atomically $ if n == bufferSize buf
      then signalNotify () semsrc
      else do
         let buf' = Buffer (bufferSize buf - n)
                           (bufferPtr buf `plusPtr` fromIntegral n)
                           
         TList.append_ (buf',semsrc) (outputBuffers s)
   
newOutputState :: FileDescriptor -> IO OutputState
newOutputState fd = do
   req <- atomically TList.empty
   return $ OutputState req fd

defaultTerminal :: Sys Terminal
defaultTerminal = do
   -- switch to non-blocking modes
   let flgs = BitSet.fromList [ HandleNonBlocking
                              , HandleCloseOnExec
                              ]
   setHandleFlags stdin  flgs
   setHandleFlags stdout flgs

   -- TODO: set terminal buffering mode?

   -- input
   inState <- lift $ newInputState (16 * 1024) stdin
   lift $ void $ forkIO $ inputThread inState

   -- output
   outState <- lift $ newOutputState stdout
   lift $ void $ forkIO $ outputThread outState

   return $ Terminal outState inState

writeToHandle :: OutputState -> Word64 -> Ptr () -> IO (TNotify ())
writeToHandle s sz ptr = atomically $ do
   (sem,semsrc) <- newTNotify
   TList.prepend_ (Buffer sz ptr, semsrc) (outputBuffers s)
   return sem

-- | Write bytes
writeTermBytes :: Terminal -> Word64 -> Ptr a -> IO (TNotify ())
writeTermBytes term sz ptr = writeToHandle (termOut term) sz (castPtr ptr)

-- | Write a string
writeStrLn :: Terminal -> String -> Sys ()
writeStrLn term s =
   sysIO $ withCStringLen s $ \(ptr,len) -> do
      sem <- writeTermBytes term (fromIntegral len) (castPtr ptr)
      atomically (waitNotify sem)

-- | Read bytes
readTermBytes :: Terminal -> Word64 -> Ptr a -> IO (TNotify ())
readTermBytes term sz ptr = readFromHandle (termIn term) sz (castPtr ptr)

readChar :: Terminal -> Sys Word8
readChar term = sysIO $
   alloca $ \(ptr :: Ptr Word8) -> do
      sem <- readTermBytes term 1 ptr
      atomically $ waitNotify sem
      peek ptr

-- | Wait for a key to pressed
waitForKey :: Terminal -> Sys ()
waitForKey term = void (readChar term)

