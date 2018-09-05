{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


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
import Control.Monad.State

import Haskus.Utils.Monad
import Haskus.Utils.STM
import Haskus.Utils.STM.Future
import Haskus.Utils.Flow
import Haskus.Utils.Variant
import Haskus.Format.Text ((%),stext, textFormat,Text,pack,shown)
import qualified Haskus.Format.Text as Text

------------------------------------------------
-- Sys monad
------------------------------------------------

-- | Sys monad
--
-- The monad that permits fun system programming:
--  * includes an optional logger
newtype Sys a
   = Sys (StateT SysState IO a)
   deriving (Monad, Applicative, Functor, MonadState SysState, MonadIO, MonadInIO)

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
      e = LogEntry "Log root" LogInfo (LogNext status log)
      initState = SysState
         { sysLogRoot    = e 
         , sysLogCurrent = logSrc
         , sysLogStatus  = statusSrc
         , sysLogGroups  = []
         }

   evalStateT act initState

-- | Fork the log in the Sys monad
forkSys :: Text -> Sys a -> Sys (IO a)
forkSys name act = do
   (status,statusSrc) <- newFutureIO
   (log,logSrc)       <- newFutureIO

   (status2,statusSrc2) <- newFutureIO
   (log2,logSrc2)       <- newFutureIO

   mainState <- get

   let
      e = LogFork name (LogNext status log) (LogNext status2 log2)

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
sysLog :: LogType -> Text -> Sys ()
sysLog typ text = sysLogAdd (LogEntry text typ)

-- | Add a new sequence of actions to the log
sysLogSequence :: Text -> Sys a -> Sys a
sysLogSequence text act = do
   sysLogBegin text
   r <- act
   sysLogEnd
   return r


-- | Start a new log sequence
sysLogBegin :: Text -> Sys ()
sysLogBegin text = do
   (log,logSrc) <- newFutureIO
   sysLogAdd (LogGroup text log)

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
      liftIO $ do
         Text.putStrLn ""
         printLog 0 log
   where
      printLog i l = 
         case l of
            LogEntry t ty (LogNext status n) -> do
               status' <- pollFutureIO status >>= \case
                  Just st -> return $ textFormat ("(" % shown % ")") st
                  Nothing -> return $ ""

               Text.putStrLn $ textFormat (stext % "---- " % stext % stext % stext)
                  (Text.replicate i "  |")
                  (case ty of
                      LogWarning  -> "Warning: "
                      LogError    -> "Error: "
                      LogDebug    -> "Debug: "
                      LogInfo     -> ""
                  )
                  t
                  status'
               mapM_ (printLog i) =<< pollFutureIO n

            LogGroup t fl (LogNext _ n) -> do
               Text.putStrLn $ textFormat (stext % "--+- " % stext)
                  (Text.replicate i "  |")
                  t
               mapM_ (printLog (i+1)) =<< pollFutureIO n
               mapM_ (printLog i)     =<< pollFutureIO fl

            LogFork t (LogNext _ n1) (LogNext _ n2) -> do
               Text.putStrLn $ textFormat (stext % "--*- FORK: " % stext)
                  (Text.replicate i "  |")
                  t
               mapM_ (printLog (i+1)) =<< pollFutureIO n2
               mapM_ (printLog i)     =<< pollFutureIO n1

-- | Assert in Sys (log the success)
sysAssert :: Text -> Bool -> Sys ()
sysAssert text b = if b
   then do
      let msg = textFormat (stext % " (success)") text
      sysLog LogInfo msg
   else do
      let msg = textFormat (stext % " (assertion failed)") text
      sysError msg

-- | Assert in Sys (don't log on success)
sysAssertQuiet :: Text -> Bool -> Sys ()
sysAssertQuiet text b = unless b $ do
   let msg = textFormat (stext % " (assertion failed)") text
   sysError msg

-- | Fail in Sys
sysError :: Text -> Sys a
sysError text = do
   sysLog LogError text
   sysOnError

-- | Log Warning in Sys
sysWarning :: Text -> Sys ()
sysWarning text = do
   sysLog LogWarning text

-- | Log Info in Sys
sysLogInfo :: Text -> Sys ()
sysLogInfo text = do
   sysLog LogInfo text

-- | Fail in Sys
sysErrorShow :: Show a => Text -> a -> Sys b
sysErrorShow text a = sysError (text <> ": " <> pack (show a))

-- | Warning in Sys
sysWarningShow :: Show a => Text -> a -> Sys ()
sysWarningShow text a = sysWarning (text <> ": " <> pack (show a))

-- | Log Info in Sys
sysLogInfoShow :: Show a => Text -> a -> Sys ()
sysLogInfoShow text a = sysLogInfo (text <> ": " <> pack (show a))

----------------------
-- Flow helpers
----------------------

-- | Assert a successful result, and log the error otherwise
flowAssertQuiet :: (Show (V xs)) => Text -> Flow Sys (a ': xs) -> Sys a
flowAssertQuiet text v = 
   v >..~!!> (\a -> sysError (textFormat (stext % " (failed with " % shown % ")") text a))

-- | Assert a successful result, log on error and on success
flowAssert :: (Show a, Show (V xs)) => Text -> Flow Sys (a ': xs) -> Sys a
flowAssert text v = 
   v  >.~=>   (\a -> sysLog LogInfo (textFormat (stext % " (succeeded with " % shown % ")") text a))
      >..~!!> (\xs -> sysError (textFormat (stext % " (failed with " % shown % ")") text xs))
     
assertShow :: Show a => Text -> a -> Sys ()
assertShow text v = do
   let msg = textFormat (stext % " (failed with " % shown % ")") text v
   sysError msg

warningShow :: Show (V xs) => Text -> Flow Sys (a ': xs) -> Sys ()
warningShow text f = do
   f >..~!> (\v ->
      sysWarning (textFormat (stext % " (failed with " % shown % ")") text v))
