module ViperVM.Arch.Linux.System.KernelEvent
   ( KernelEvent(..)
   , KernelEventAction(..)
   , createKernelEventSocket
   , receiveKernelEvent
   )
where

import ViperVM.Arch.X86_64.Linux.Network
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Network.SendReceive

import Control.Monad.Trans.Either
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as UTF8
import Data.List.Split

data KernelEventAction
   = ActionAdd
   | ActionRemove
   | ActionChange
   | ActionOnline
   | ActionOffline
   | ActionMove
   | ActionOther String
   deriving (Show)

data KernelEvent = KernelEvent
   { kernelEventAction    :: KernelEventAction
   , kernelEventDevPath   :: String
   , kernelEventSubSystem :: String
   , kernelEventFields    :: Map.Map String String
   } deriving Show

createKernelEventSocket :: SysRet FileDescriptor
createKernelEventSocket = runEitherT $ do
   fd <- EitherT $ sysSocket (SockTypeNetlink NetlinkTypeKernelEvent) []
   EitherT $ sysBindNetlink fd 0 0xFFFFFFFF
   return fd


receiveKernelEvent :: FileDescriptor -> SysRet KernelEvent
receiveKernelEvent fd = runEitherT $ do
   msg <- parseKernelEvent <$> EitherT (receiveByteString fd 2048 [])
   case msg of
      Nothing -> EitherT (receiveKernelEvent fd)
      Just m  -> return m

parseKernelEvent :: BS.ByteString -> Maybe KernelEvent
parseKernelEvent bs = r
   where
      bss = fmap UTF8.toString (BS.split 0 bs)
      r = case bss of
         ("libudev":_) -> Nothing
         _             -> Just ke
      toTuple (x:y:_) = (x,y)
      toTuple x = error $ "Invalid tuple: " ++ show x
      fs = Map.fromList . fmap (toTuple . splitOn "=") . filter (/="")  $ tail bss
      act = case fs Map.! "ACTION" of
         "add"    -> ActionAdd
         "remove" -> ActionRemove
         "change" -> ActionChange
         "online" -> ActionOnline
         "offline"-> ActionOffline
         "move"   -> ActionMove
         x        -> ActionOther x
      devpath = fs Map.! "DEVPATH"
      subsys = fs Map.! "SUBSYSTEM"
      fs' = Map.delete "SUBSYSTEM" (Map.delete "ACTION" (Map.delete "DEVPATH" fs))
      ke = KernelEvent act devpath subsys fs'
