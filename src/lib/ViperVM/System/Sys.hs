{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ViperVM.System.Sys
   ( Sys
   , runSys
   , runSys'
   , sysIO
   , sysIO'
   , sysWith
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
   )
where

import Prelude hiding (log)
import Data.Text.Lazy as Text
import Data.Text.Lazy.IO as Text
import Data.String (fromString)
import qualified Data.Text.Format as Text
import Control.Monad.State
import Data.Foldable (traverse_)
import Text.Printf
import ViperVM.Utils.STM.Future
import Control.Concurrent.STM

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

-- | Run and return nothing
runSys' :: Sys a -> IO ()
runSys' = void . runSys

-- | Execute an IO action that may use the state
sysIO' :: (SysState -> IO (a,SysState)) -> Sys a
sysIO' = Sys . StateT

-- | Execute an IO action
sysIO :: IO a -> Sys a
sysIO = Sys . liftIO

-- | Lift with* and alloca* functions
sysWith :: (forall c. (a -> IO c) -> IO c) -> (a -> Sys b) -> Sys b
sysWith wth f =
   sysIO' $ \s ->
      wth $ \a ->
         sysRun s (f a)

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

data LogStatus
   = LogSuccess
   | LogFailed
   deriving (Show,Eq)

data LogType
   = LogDebug
   | LogInfo
   | LogWarning
   | LogError
   deriving (Show,Eq)


setLogStatus :: LogStatus -> Sys ()
setLogStatus s = do
   st <- gets sysLogStatus
   sysIO (setFutureIO s st)

sysLogAdd :: (LogNext -> Log) -> Sys ()
sysLogAdd f = do
   (status,statusSrc) <- sysIO newFutureIO
   (log,logSrc)       <- sysIO newFutureIO
   let e = f (LogNext status log)

   -- link with previous entry
   c <- gets sysLogCurrent
   sysIO $ atomically (setFuture e c)

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
   (log,logSrc) <- sysIO newFutureIO
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

               Text.putStrLn $ Text.format (fromString "{}---- {}{}{}")
                  ( Text.replicate i (Text.pack "  |")
                  , Text.pack $ case ty of
                     LogWarning  -> "Warning: "
                     LogError    -> "Error: "
                     LogDebug    -> "Debug: "
                     LogInfo     -> ""
                  , t
                  , status'
                  )
               traverse_ (printLog i) =<< pollFutureIO n

            LogGroup t fl (LogNext _ n) -> do
               Text.putStrLn $ Text.format (fromString "{}--+- {}")
                  ( Text.replicate i (Text.pack "  |")
                  , t
                  )
               traverse_ (printLog (i+1)) =<< pollFutureIO n
               traverse_ (printLog i)     =<< pollFutureIO fl

            LogFork t (LogNext _ n1) (LogNext _ n2) -> do
               Text.putStrLn $ Text.format (fromString "{}--*- FORK: {}")
                  ( Text.replicate i (Text.pack "  |")
                  , t
                  )
               traverse_ (printLog (i+1)) =<< pollFutureIO n2
               traverse_ (printLog i)     =<< pollFutureIO n1

sysAssert :: String -> Bool -> Sys ()
sysAssert text b = if b
   then do
      let msg = printf "%s (success)" text
      sysLog LogInfo msg
   else do
      let msg = printf "%s (assertion failed)" text
      sysError msg

sysAssertQuiet :: String -> Bool -> Sys ()
sysAssertQuiet text b = unless b $ do
   let msg = printf "%s (assertion failed)" text
   sysError msg

sysError :: String -> Sys a
sysError text = do
   sysLog LogError text
   sysOnError

