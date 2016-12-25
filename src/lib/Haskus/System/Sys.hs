{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Sys monad
module Haskus.System.Sys
   ( Sys
   , runSys
   , runSys'
   , forkSys
   , sysRun
   , sysRun'
   , sysExec
   , Log (..)
   , LogType (..)
   , sysLog
   , sysLogPrint
   , sysLogSequence
   , setLogStatus
   , sysAssert
   , sysAssertQuiet
   , sysError
   , sysWarning
   , sysLogInfo
   , sysErrorShow
   , sysWarningShow
   , sysLogInfoShow
   -- ** Flow helpers
   , flowAssertQuiet
   , flowAssert
   , assertShow
   , warningShow
   )
where

import Prelude hiding (log)
import Data.String (fromString)
import Control.Monad.State
import Text.Printf

import Haskus.Utils.Monad
import Haskus.Utils.STM
import Haskus.Utils.STM.Future
import Haskus.Utils.Flow
import Haskus.Utils.Variant
import Haskus.Format.Text as Text

------------------------------------------------
-- Sys monad
------------------------------------------------

-- | Sys monad
--
-- The monad that permits fun system programming:
--  * includes an optional logger
newtype Sys a = Sys (StateT SysState IO a) deriving (Monad,Applicative,Functor,MonadState SysState, MonadIO)

data SysState = SysState
   { sysLogRoot    :: Log                    -- ^ Root of the log
   , sysLogCurrent :: FutureSource Log       -- ^ Current log
   , sysLogStatus  :: FutureSource LogStatus -- ^ Status
   , sysLogGroups  :: [FutureSource Log]     -- ^ Stack for groups
   }

-- | Run
runSys :: Sys a -> IO a
runSys (Sys act) = do
   (status,statusSrc) <- newFutureIO
   (log,logSrc)       <- newFutureIO

   let
      e = LogEntry (Text.pack "Log root") LogInfo (LogNext status log)
      initState = SysState
         { sysLogRoot    = e 
         , sysLogCurrent = logSrc
         , sysLogStatus  = statusSrc
         , sysLogGroups  = []
         }

   evalStateT act initState

-- | Fork the log in the Sys monad
forkSys :: String -> Sys a -> Sys (IO a)
forkSys name act = do
   (status,statusSrc) <- newFutureIO
   (log,logSrc)       <- newFutureIO

   (status2,statusSrc2) <- newFutureIO
   (log2,logSrc2)       <- newFutureIO

   mainState <- get

   let
      e = LogFork (Text.pack name) (LogNext status log) (LogNext status2 log2)

      forkState = SysState
         { sysLogRoot    = sysLogRoot mainState
         , sysLogCurrent = logSrc2
         , sysLogStatus  = statusSrc2
         , sysLogGroups  = []
         }

   -- link with previous entry
   c <- gets sysLogCurrent
   atomically (setFuture e c)

   -- set main state
   put $ mainState
            { sysLogCurrent = logSrc
            , sysLogStatus  = statusSrc
            }

   let (Sys act') = do
            v <- act
            sysLog LogInfo "Fork ended"
            return v

   return (evalStateT act' forkState)

-- | Run and return nothing
runSys' :: Sys a -> IO ()
runSys' = void . runSys

instance MonadInIO Sys where
   {-# INLINE liftWith #-}
   liftWith  = sysWith

   {-# INLINE liftWith2 #-}
   liftWith2 = sysWith2

-- | Lift with* and alloca* functions
sysWith :: (forall c. (a -> IO c) -> IO c) -> (a -> Sys b) -> Sys b
sysWith wth f =
   Sys . StateT $ \s ->
      wth $ \a ->
         sysRun s (f a)

-- | Lift with* and alloca* functions
sysWith2 :: (forall c. (a -> b -> IO c) -> IO c) -> (a -> b -> Sys e) -> Sys e
sysWith2 wth f =
   Sys . StateT $ \s ->
      wth $ \a b ->
         sysRun s (f a b)

-- | Run with an explicit state
sysRun :: SysState -> Sys a -> IO (a, SysState)
sysRun s (Sys f) = runStateT f s

-- | Run with an explicit state
sysRun' :: SysState -> Sys a -> IO ((), SysState)
sysRun' s (Sys f) = do
   (_, s2) <- runStateT f s
   return ((),s2)


-- | Exec with an explicit state
sysExec :: SysState -> Sys a -> IO SysState
sysExec s f = snd <$> sysRun s f

-- | Called on system error
sysOnError :: Sys a
sysOnError = do
   -- print the log
   sysLogPrint

   -- fail
   error "System failed"

------------------------------------------------
-- Logging
------------------------------------------------

-- | Hierarchical thread-safe log
data Log
   = LogEntry Text LogType       LogNext
   | LogGroup Text (Future Log)  LogNext
   | LogFork  Text               LogNext LogNext

-- | Status of the current entry and link to the following one
data LogNext = LogNext (Future LogStatus) (Future Log)

-- | Status
data LogStatus
   = LogSuccess
   | LogFailed
   deriving (Show,Eq)

-- | Log type
data LogType
   = LogDebug
   | LogInfo
   | LogWarning
   | LogError
   deriving (Show,Eq)

-- | Set log status
setLogStatus :: LogStatus -> Sys ()
setLogStatus s = do
   st <- gets sysLogStatus
   setFutureIO s st

-- | Add a log entry
sysLogAdd :: (LogNext -> Log) -> Sys ()
sysLogAdd f = do
   (status,statusSrc) <- newFutureIO
   (log,logSrc)       <- newFutureIO
   let e = f (LogNext status log)

   -- link with previous entry
   c <- gets sysLogCurrent
   atomically (setFuture e c)

   -- update state
   modify' $ \s -> s
      { sysLogStatus  = statusSrc
      , sysLogCurrent = logSrc
      }

-- | Add a new entry to the log
sysLog :: LogType -> String -> Sys ()
sysLog typ text = sysLogAdd (LogEntry (Text.pack text) typ)

-- | Add a new sequence of actions to the log
sysLogSequence :: String -> Sys a -> Sys a
sysLogSequence text act = do
   sysLogBegin text
   r <- act
   sysLogEnd
   return r


-- | Start a new log sequence
sysLogBegin :: String -> Sys ()
sysLogBegin text = do
   (log,logSrc) <- newFutureIO
   sysLogAdd (LogGroup (Text.pack text) log)

   -- add the group to the list
   modify' $ \s -> s
      { sysLogGroups = logSrc : sysLogGroups s
      }

-- | End a log sequence
sysLogEnd :: Sys ()
sysLogEnd =
   modify' $ \s -> s
      { sysLogGroups  = Prelude.tail (sysLogGroups s)
      , sysLogCurrent = Prelude.head (sysLogGroups s)
      }

-- | Print the log on the standard output
-- FIXME: use System.Terminal
sysLogPrint :: Sys ()
sysLogPrint = do
      -- print the log
      log <- gets sysLogRoot
      liftIO $ printLog 0 log
   where
      printLog i l = 
         case l of
            LogEntry t ty (LogNext status n) -> do
               status' <- pollFutureIO status >>= \case
                  Just st -> return $ Text.pack ("("++show st++")")
                  Nothing -> return $ Text.pack ""

               Text.putStrLn $ textFormat (fromString "{}---- {}{}{}")
                  ( Text.replicate i (Text.pack "  |")
                  , Text.pack $ case ty of
                     LogWarning  -> "Warning: "
                     LogError    -> "Error: "
                     LogDebug    -> "Debug: "
                     LogInfo     -> ""
                  , t
                  , status'
                  )
               mapM_ (printLog i) =<< pollFutureIO n

            LogGroup t fl (LogNext _ n) -> do
               Text.putStrLn $ textFormat (fromString "{}--+- {}")
                  ( Text.replicate i (Text.pack "  |")
                  , t
                  )
               mapM_ (printLog (i+1)) =<< pollFutureIO n
               mapM_ (printLog i)     =<< pollFutureIO fl

            LogFork t (LogNext _ n1) (LogNext _ n2) -> do
               Text.putStrLn $ textFormat (fromString "{}--*- FORK: {}")
                  ( Text.replicate i (Text.pack "  |")
                  , t
                  )
               mapM_ (printLog (i+1)) =<< pollFutureIO n2
               mapM_ (printLog i)     =<< pollFutureIO n1

-- | Assert in Sys (log the success)
sysAssert :: String -> Bool -> Sys ()
sysAssert text b = if b
   then do
      let msg = printf "%s (success)" text
      sysLog LogInfo msg
   else do
      let msg = printf "%s (assertion failed)" text
      sysError msg

-- | Assert in Sys (don't log on success)
sysAssertQuiet :: String -> Bool -> Sys ()
sysAssertQuiet text b = unless b $ do
   let msg = printf "%s (assertion failed)" text
   sysError msg

-- | Fail in Sys
sysError :: String -> Sys a
sysError text = do
   sysLog LogError text
   sysOnError

-- | Log Warning in Sys
sysWarning :: String -> Sys ()
sysWarning text = do
   sysLog LogWarning text

-- | Log Info in Sys
sysLogInfo :: String -> Sys ()
sysLogInfo text = do
   sysLog LogInfo text

-- | Fail in Sys
sysErrorShow :: Show a => String -> a -> Sys b
sysErrorShow text a = sysError (text ++ ": " ++ show a)

-- | Warning in Sys
sysWarningShow :: Show a => String -> a -> Sys ()
sysWarningShow text a = sysWarning (text ++ ": " ++ show a)

-- | Log Info in Sys
sysLogInfoShow :: Show a => String -> a -> Sys ()
sysLogInfoShow text a = sysLogInfo (text ++ ": " ++ show a)

----------------------
-- Flow helpers
----------------------

-- | Assert a successful result, and log the error otherwise
flowAssertQuiet :: (Show (Variant xs)) => String -> Flow Sys (a ': xs) -> Sys a
flowAssertQuiet text v = 
   v >..~!!> (\a -> sysError (printf "%s (failed with %s)" text (show a)))

-- | Assert a successful result, log on error and on success
flowAssert :: (Show a, Show (Variant xs)) => String -> Flow Sys (a ': xs) -> Sys a
flowAssert text v = 
   v  >.~=>   (\a -> sysLog LogInfo (printf "%s (succeeded with %s)" text (show a)))
      >..~!!> (\xs -> sysError (printf "%s (failed with %s)" text (show xs)))
     
assertShow :: Show a => String -> a -> Sys ()
assertShow text v = do
   let msg = printf "%s (failed with %s)" text (show v)
   sysError msg

warningShow :: Show (Variant xs) => String -> Flow Sys (a ': xs) -> Sys ()
warningShow text f = do
   f >..~!> (\v ->
      sysWarning (printf "%s (failed with %s)" text (show v)))
