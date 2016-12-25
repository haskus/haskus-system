{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Notifications on file system (poll, select, inotify, etc.)
module Haskus.Arch.Linux.FileSystem.Notification
   ( PollEvent(..)
   , PollEventSet
   , PollEntry(..)
   , PollResult(..)
   , sysPoll
   )
where

import Haskus.Utils.Maybe (mapMaybe)
import Haskus.Utils.Types.Generics (Generic)
import Haskus.Utils.Flow
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.BitSet (CBitSet, BitSet, fromBits, toBits)
import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Syscalls

-- | Poll struct
data PollStruct = PollStruct
   { pollFD             :: Int32
   , pollEvents         :: Word16
   , pollReturnedEvents :: Word16
   } deriving (Generic,Storable)

-- | Polling event
data PollEvent
   = PollReadable
   | PollWritable
   | PollPriorityReadable
   | PollError
   | PollHungUp
   | PollInvalidHandle
   | PollMessage
   | PollRemove
   | PollPeerHungUp
   | PollReadNormal
   | PollWriteNormal
   | PollReadBand
   | PollWriteBand
   deriving (Show,Eq,CBitSet)

instance Enum PollEvent where
   fromEnum x = case x of
      PollReadable               -> 0
      PollWritable               -> 2
      PollPriorityReadable       -> 1
      PollError                  -> 3
      PollHungUp                 -> 4
      PollInvalidHandle          -> 5
      PollMessage                -> 10
      PollRemove                 -> 12
      PollPeerHungUp             -> 13
      PollReadNormal             -> 6
      PollWriteNormal            -> 8
      PollReadBand               -> 7
      PollWriteBand              -> 9
   toEnum x = case x of
      0  -> PollReadable               
      2  -> PollWritable               
      1  -> PollPriorityReadable       
      3  -> PollError                  
      4  -> PollHungUp                 
      5  -> PollInvalidHandle  
      10 -> PollMessage                
      12 -> PollRemove                 
      13 -> PollPeerHungUp             
      6  -> PollReadNormal             
      8  -> PollWriteNormal            
      7  -> PollReadBand               
      9  -> PollWriteBand              
      _  -> error "Unknown poll event"

-- | A set of polling events
type PollEventSet = BitSet Word16 PollEvent

-- | A polling entry
data PollEntry = PollEntry Handle PollEventSet deriving (Show,Eq)

-- | Result of a call to poll
data PollResult
   = PollTimeOut              -- ^ Time out
   | PollEvents [PollEntry]   -- ^ Events returned
   deriving (Show,Eq)

-- | Poll a set of file descriptors
--
-- Timeout in milliseconds
sysPoll :: MonadInIO m => [PollEntry] -> Bool -> Maybe Int64 -> Flow m '[PollResult,ErrorCode]
sysPoll entries blocking timeout = do
   
   let 
      toPollStruct (PollEntry (Handle fd) evs) = PollStruct
         { pollFD             = fromIntegral fd -- poll allows negative FDs to indicate that the entry must be skipped, we don't
         , pollEvents         = toBits evs
         , pollReturnedEvents = 0
         }
      fromPollStruct (PollStruct fd _ evs) = 
         if evs == 0
            then Nothing
            else Just $ PollEntry (Handle (fromIntegral fd)) (fromBits evs)
      fds = fmap toPollStruct entries
      nfds = fromIntegral (length fds) :: Word64
      timeout' = if not blocking
         then 0
         else case timeout of
            Nothing -> -1 -- infinite blocking
            Just x  -> abs x
   
   withArray fds $ \fds' -> do
      liftIO (syscall @"poll" (castPtr fds') nfds timeout')
         ||> toErrorCode
         >.~.> (\case
            0 -> return PollTimeOut
            _ -> do
               retfds <- peekArray (fromIntegral (length fds)) fds'
               return (PollEvents $ mapMaybe fromPollStruct retfds))

