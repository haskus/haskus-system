{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.System.Linux.Network
   ( sysShutdown
   , sysSendFile
   , sysSendFileWithOffset
   , SocketProtocol(..)
   , SocketRawType(..)
   , SocketType(..)
   , IPType(..)
   , sysSocket'
   , sysSocket
   , sysSocketPair'
   , sysSocketPair
   , sysBind
   , sysBindNetlink
   , sysConnect
   , sysAccept
   , sysListen
   )
where

import Haskus.Utils.List (foldl')
import Haskus.Utils.Flow
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Syscalls
import Haskus.System.Linux.Internals.Netlink
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Bits
import Foreign.Ptr

data ShutFlag
   = ShutRead
   | ShutWrite
   | ShutReadWrite
   deriving (Enum,Show)

-- | Shut down part of a full-duplex connection
sysShutdown :: MonadIO m => Handle -> ShutFlag -> Excepts '[ErrorCode] m ()
sysShutdown (Handle fd) flag =
   checkErrorCode_ =<< liftIO (syscall_shutdown fd (fromEnum flag))

-- | Call sendfile using implicit file cursor for input
sysSendFile :: MonadIO m => Handle -> Handle -> Word64 -> Excepts '[ErrorCode] m Word64
sysSendFile (Handle outfd) (Handle infd) count = do
   r <- checkErrorCode =<< liftIO (syscall_sendfile outfd infd nullPtr count)
   return (fromIntegral r)

-- | Call sendFile using explicit input offset, returns new offset
sysSendFileWithOffset :: MonadInIO m => Handle -> Handle -> Word64 -> Word64 -> Excepts '[ErrorCode] m (Word64,Word64)
sysSendFileWithOffset (Handle outfd) (Handle infd) offset count =
   with offset $ \off -> do
      x <- checkErrorCode =<< liftIO (syscall_sendfile outfd infd off count)
      newOff <- peek off
      return (fromIntegral x, newOff)

-- | Socket protocol family
data SocketProtocol
   = SockProtUNSPEC     -- ^ Unspecified (0)
   | SockProtLOCAL      -- ^ Local to host (pipes and file-domain). Also called UNIX or FILE
   | SockProtIPv4       -- ^ IP protocol family
   | SockProtAX25       -- ^ Amateur Radio AX.25
   | SockProtIPX        -- ^ Novell Internet Protocol
   | SockProtAPPLETALK  -- ^ Appletalk DDP
   | SockProtNETROM     -- ^ Amateur radio NetROM
   | SockProtBRIDGE     -- ^ Multiprotocol bridge
   | SockProtATMPVC     -- ^ ATM PVCs
   | SockProtX25        -- ^ Reserved for X.25 project
   | SockProtIPv6       -- ^ IP version 6
   | SockProtROSE       -- ^ Amateur Radio X.25 PLP
   | SockProtDECnet     -- ^ Reserved for DECnet project
   | SockProtNETBEUI    -- ^ Reserved for 802.2LLC project
   | SockProtSECURITY   -- ^ Security callback pseudo AF
   | SockProtKEY        -- ^ PF_KEY key management API
   | SockProtNETLINK    -- ^ Netlink. Also called ROUTE
   | SockProtPACKET     -- ^ Packet family
   | SockProtASH        -- ^ Ash
   | SockProtECONET     -- ^ Acorn Econet
   | SockProtATMSVC     -- ^ ATM SVCs
   | SockProtRDS        -- ^ RDS sockets
   | SockProtSNA        -- ^ Linux SNA Project
   | SockProtIRDA       -- ^ IRDA sockets
   | SockProtPPPOX      -- ^ PPPoX sockets
   | SockProtWANPIPE    -- ^ Wanpipe API sockets
   | SockProtLLC        -- ^ Linux LLC
   | SockProt27         -- ^ Unknown. Added to simplify Enum deriving
   | SockProt28         -- ^ Unknown. Added to simplify Enum deriving
   | SockProtCAN        -- ^ Controller Area Network
   | SockProtTIPC       -- ^ TIPC sockets
   | SockProtBLUETOOTH  -- ^ Bluetooth sockets
   | SockProtIUCV       -- ^ IUCV sockets
   | SockProtRXRPC      -- ^ RxRPC sockets
   | SockProtISDN       -- ^ mISDN sockets
   | SockProtPHONET     -- ^ Phonet sockets
   | SockProtIEEE802154 -- ^ IEEE 802.15.4 sockets
   | SockProtCAIF       -- ^ CAIF sockets
   | SockProtALG        -- ^ Algorithm sockets
   | SockProtNFC        -- ^ NFC sockets
   | SockProtVSOCK      -- ^ vSockets
   deriving (Enum, Eq, Show)

data SocketRawType
   = SockRawTypeStream     -- ^ Sequenced, reliable, connection-based byte streams
   | SockRawTypeDatagram   -- ^ Connectionless, unreliable datagrams of fixed maximum length
   | SockRawTypeRaw        -- ^ Raw protocol interface
   | SockRawTypeRDM        -- ^ Reliably delivered messages
   | SockRawTypeSeqPacket  -- ^ Sequenced, reliable, connection-based, datagrams of fixed maximum length
   | SockRawTypeDCCP       -- ^ Datagram congestion control protocol
   | SockRawTypePacket     -- ^ Linux specific way of getting packets at the dev
                           --   level. For writing rarp and other similar things on the user level
   deriving (Show,Eq)

instance Enum SocketRawType where
   toEnum x = case x of
      1  -> SockRawTypeStream
      2  -> SockRawTypeDatagram
      3  -> SockRawTypeRaw
      4  -> SockRawTypeRDM
      5  -> SockRawTypeSeqPacket
      6  -> SockRawTypeDCCP
      10 -> SockRawTypePacket
      _  -> error "Invalid socket type"

   fromEnum x = case x of
      SockRawTypeStream    -> 1
      SockRawTypeDatagram  -> 2
      SockRawTypeRaw       -> 3
      SockRawTypeRDM       -> 4
      SockRawTypeSeqPacket -> 5
      SockRawTypeDCCP      -> 6
      SockRawTypePacket    -> 10

-- | Socket option
data SocketOption
   = SockOptCloseOnExec    -- ^ Atomically set close-on-exec flag
   | SockOptNonBlocking    -- ^ Atomically mark descriptor as non-blocking
   deriving (Show,Eq)

instance Enum SocketOption where
   toEnum 0x80000 = SockOptCloseOnExec
   toEnum 0x800   = SockOptNonBlocking
   toEnum _       = error "Invalid socket option"

   fromEnum SockOptCloseOnExec = 0x80000
   fromEnum SockOptNonBlocking = 0x800

-- | Create a socket (low-level API)
--
-- `subprotocol` may be 0
sysSocket' :: MonadIO m => SocketRawType -> SocketProtocol -> Int -> [SocketOption] -> Excepts '[ErrorCode] m Handle
sysSocket' typ protocol subprotocol opts = do
   let
      f :: Enum a => a -> Word64
      f = fromIntegral . fromEnum
      typ' = f typ .|. foldl' (\x y -> x .|. f y) 0 opts
   r <- checkErrorCode =<< liftIO (syscall_socket (fromEnum protocol) typ' subprotocol)
   return (Handle (fromIntegral r))

-- | Create a socket pair (low-level API)
--
-- `subprotocol` may be 0
sysSocketPair' :: MonadInIO m => SocketRawType -> SocketProtocol -> Int -> [SocketOption] -> Excepts '[ErrorCode] m (Handle,Handle)
sysSocketPair' typ protocol subprotocol opts =
   allocaArray 2 $ \ptr -> do
      let
         f :: Enum a => a -> Word64
         f = fromIntegral . fromEnum
         typ' = f typ .|. foldl' (\x y -> x .|. f y) 0 opts
         toTuple [x,y] = (x,y)
         toTuple _     = error "Invalid tuple"

      checkErrorCode_ =<< liftIO (syscall_socketpair (fromEnum protocol) typ' subprotocol (castPtr ptr))
      (toTuple . fmap Handle) <$> peekArray 2 ptr

-- | IP type
data IPType
   = IPv4
   | IPv6
   deriving (Show,Eq)

-- | Socket type
data SocketType
   = SockTypeTCP IPType
   | SockTypeUDP IPType
   | SockTypeNetlink NetlinkType
   deriving (Show,Eq)

-- | Create a socket
sysSocket :: MonadIO m => SocketType -> [SocketOption] -> Excepts '[ErrorCode] m Handle
sysSocket typ opts =
   case typ of
      SockTypeTCP IPv4   -> sysSocket' SockRawTypeStream SockProtIPv4 0 opts
      SockTypeTCP IPv6   -> sysSocket' SockRawTypeStream SockProtIPv6 0 opts
      SockTypeUDP IPv4   -> sysSocket' SockRawTypeDatagram SockProtIPv4 0 opts
      SockTypeUDP IPv6   -> sysSocket' SockRawTypeDatagram SockProtIPv6 0 opts
      SockTypeNetlink nt -> sysSocket' SockRawTypeDatagram SockProtNETLINK (fromEnum nt) opts

-- | Create a socket pair
sysSocketPair :: MonadInIO m => SocketType -> [SocketOption] -> Excepts '[ErrorCode] m (Handle,Handle)
sysSocketPair typ opts =
   case typ of
      SockTypeTCP IPv4   -> sysSocketPair' SockRawTypeStream SockProtIPv4 0 opts
      SockTypeTCP IPv6   -> sysSocketPair' SockRawTypeStream SockProtIPv6 0 opts
      SockTypeUDP IPv4   -> sysSocketPair' SockRawTypeDatagram SockProtIPv4 0 opts
      SockTypeUDP IPv6   -> sysSocketPair' SockRawTypeDatagram SockProtIPv6 0 opts
      SockTypeNetlink nt -> sysSocketPair' SockRawTypeDatagram SockProtNETLINK (fromEnum nt) opts

-- | Bind a socket
sysBind :: (MonadInIO m, Storable a) => Handle -> a -> Excepts '[ErrorCode] m ()
sysBind (Handle fd) addr =
   with addr $ \addr' ->
      checkErrorCode_ =<< liftIO (syscall_bind fd (castPtr addr') (sizeOf' addr))

-- | Connect a socket
sysConnect :: (MonadInIO m, Storable a) => Handle -> a -> Excepts '[ErrorCode] m ()
sysConnect (Handle fd) addr =
   with addr $ \addr' ->
      checkErrorCode_ =<< liftIO (syscall_connect fd (castPtr addr') (sizeOf' addr))

-- | Accept a connection on a socket
--
-- We use accept4 (288) instead of accept (43) to support socket options
--
sysAccept :: (MonadInIO m, Storable a) => Handle -> a -> [SocketOption] -> Excepts '[ErrorCode] m Handle
sysAccept (Handle fd) addr opts =
   let 
      f :: Enum a => a -> Word64
      f = fromIntegral . fromEnum
      opts' = foldl' (\x y -> x .|. f y) 0 opts
   in
   with addr $ \addr' -> do
      r <- checkErrorCode =<< liftIO (syscall_accept4 fd (castPtr addr') (sizeOf' addr) opts')
      return (Handle (fromIntegral r))

-- | Listen on a socket
--
-- @ backlog is the number of incoming requests that are stored
sysListen :: MonadIO m => Handle -> Word64 -> Excepts '[ErrorCode] m ()
sysListen (Handle fd) backlog =
   checkErrorCode_ =<< liftIO (syscall_listen fd backlog)


-- | Bind a netlink socket
--
-- @groups@ is a group mask
sysBindNetlink :: MonadInIO m => Handle -> Word32 -> Word32 -> Excepts '[ErrorCode] m ()
sysBindNetlink fd portID groups = sysBind fd <| NetlinkSocket
   { netlinkSocketFamily = fromIntegral (fromEnum SockProtNETLINK)
   , netlinkSocketPortID = portID
   , netlinkSocketGroups = groups
   }
