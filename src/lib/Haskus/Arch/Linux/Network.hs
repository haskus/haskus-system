{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.Arch.Linux.Network
   ( sysShutdown
   , sysSendFile
   , sysSendFileWithOffset
   , SocketProtocol(..)
   , SocketRawType(..)
   , SocketType(..)
   , NetlinkType(..)
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

import Haskus.Utils.Types.Generics (Generic)
import Haskus.Utils.List (foldl')
import Haskus.Utils.Flow
import Haskus.Arch.Linux.ErrorCode
import Haskus.Arch.Linux.Handle
import Haskus.Arch.Linux.Syscalls
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Ptr

data ShutFlag
   = ShutRead
   | ShutWrite
   | ShutReadWrite
   deriving (Enum,Show)

-- | Shut down part of a full-duplex connection
sysShutdown :: MonadIO m => Handle -> ShutFlag -> Flow m '[(),ErrorCode]
sysShutdown (Handle fd) flag =
   liftIO (syscall_shutdown fd (fromEnum flag))
      ||> toErrorCodeVoid

-- | Call sendfile using implicit file cursor for input
sysSendFile :: MonadIO m => Handle -> Handle -> Word64 -> Flow m '[Word64,ErrorCode]
sysSendFile (Handle outfd) (Handle infd) count =
   liftIO (syscall_sendfile outfd infd nullPtr count)
      ||> toErrorCodePure fromIntegral

-- | Call sendFile using explicit input offset, returns new offset
sysSendFileWithOffset :: MonadInIO m => Handle -> Handle -> Word64 -> Word64 -> Flow m '[(Word64,Word64),ErrorCode]
sysSendFileWithOffset (Handle outfd) (Handle infd) offset count =
   with offset $ \off -> liftIO (syscall_sendfile outfd infd off count)
      ||> toErrorCode
      >.~.> (\x -> do
         newOff <- peek off
         return (fromIntegral x, newOff))

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
sysSocket' :: MonadIO m => SocketRawType -> SocketProtocol -> Int -> [SocketOption] -> Flow m '[Handle,ErrorCode]
sysSocket' typ protocol subprotocol opts =
   liftIO (syscall_socket (fromEnum protocol) typ' subprotocol)
      ||> toErrorCodePure (Handle . fromIntegral)
   where
      f :: Enum a => a -> Word64
      f = fromIntegral . fromEnum
      typ' = f typ .|. foldl' (\x y -> x .|. f y) 0 opts

-- | Create a socket pair (low-level API)
--
-- `subprotocol` may be 0
sysSocketPair' :: MonadInIO m => SocketRawType -> SocketProtocol -> Int -> [SocketOption] -> Flow m '[(Handle,Handle),ErrorCode]
sysSocketPair' typ protocol subprotocol opts =
   allocaArray 2 $ \ptr ->
      liftIO (syscall_socketpair (fromEnum protocol) typ' subprotocol (castPtr ptr))
         ||>   toErrorCode
         >.~.> (const $ toTuple . fmap Handle <$> peekArray 2 ptr)
   where
      f :: Enum a => a -> Word64
      f = fromIntegral . fromEnum
      typ' = f typ .|. foldl' (\x y -> x .|. f y) 0 opts
      toTuple [x,y] = (x,y)
      toTuple _     = error "Invalid tuple"

-- | IP type
data IPType
   = IPv4
   | IPv6
   deriving (Show,Eq)

-- | Netlink socket type
data NetlinkType
   = NetlinkTypeRoute            -- ^ Routing/device hook
   | NetlinkTypeUserSocket       -- ^ Reserved for user mode socket protocols
   | NetlinkTypeSocketDiagnostic -- ^ socket monitoring
   | NetlinkTypeNFLOG            -- ^ netfilter/iptables ULOG
   | NetlinkTypeXFRM             -- ^ ipsec
   | NetlinkTypeSELINUX          -- ^ SELinux event notifications
   | NetlinkTypeISCSI            -- ^ Open-iSCSI
   | NetlinkTypeAudit            -- ^ auditing
   | NetlinkTypeFibLookup
   | NetlinkTypeConnector
   | NetlinkTypeNetfilter        -- ^ netfilter subsystem
   | NetlinkTypeIP6FW
   | NetlinkTypeDNRTMSG          -- ^ DECnet routing messages
   | NetlinkTypeKernelEvent      -- ^ Kernel messages to userspace
   | NetlinkTypeGeneric
   | NetlinkTypeSCSITransport    -- ^ SCSI Transports
   | NetlinkTypeECryptFS
   | NetlinkTypeRDMA
   | NetlinkTypeCrypto           -- ^ Crypto layer
   deriving (Show,Eq)

instance Enum NetlinkType where
   toEnum x = case x of
      0  -> NetlinkTypeRoute
      2  -> NetlinkTypeUserSocket
      4  -> NetlinkTypeSocketDiagnostic
      5  -> NetlinkTypeNFLOG
      6  -> NetlinkTypeXFRM
      7  -> NetlinkTypeSELINUX
      8  -> NetlinkTypeISCSI
      9  -> NetlinkTypeAudit
      10 -> NetlinkTypeFibLookup
      11 -> NetlinkTypeConnector
      12 -> NetlinkTypeNetfilter
      13 -> NetlinkTypeIP6FW
      14 -> NetlinkTypeDNRTMSG
      15 -> NetlinkTypeKernelEvent
      16 -> NetlinkTypeGeneric
      18 -> NetlinkTypeSCSITransport
      19 -> NetlinkTypeECryptFS
      20 -> NetlinkTypeRDMA
      21 -> NetlinkTypeCrypto
      _  -> error "Invalid NetLink type"

   fromEnum x = case x of
      NetlinkTypeRoute              -> 0
      NetlinkTypeUserSocket         -> 2
      NetlinkTypeSocketDiagnostic   -> 4
      NetlinkTypeNFLOG              -> 5
      NetlinkTypeXFRM               -> 6
      NetlinkTypeSELINUX            -> 7
      NetlinkTypeISCSI              -> 8
      NetlinkTypeAudit              -> 9
      NetlinkTypeFibLookup          -> 10
      NetlinkTypeConnector          -> 11
      NetlinkTypeNetfilter          -> 12
      NetlinkTypeIP6FW              -> 13
      NetlinkTypeDNRTMSG            -> 14
      NetlinkTypeKernelEvent        -> 15
      NetlinkTypeGeneric            -> 16
      NetlinkTypeSCSITransport      -> 18
      NetlinkTypeECryptFS           -> 19
      NetlinkTypeRDMA               -> 20
      NetlinkTypeCrypto             -> 21

-- | Socket type
data SocketType
   = SockTypeTCP IPType
   | SockTypeUDP IPType
   | SockTypeNetlink NetlinkType
   deriving (Show,Eq)

-- | Create a socket
sysSocket :: MonadIO m => SocketType -> [SocketOption] -> Flow m '[Handle,ErrorCode]
sysSocket typ opts =
   case typ of
      SockTypeTCP IPv4   -> sysSocket' SockRawTypeStream SockProtIPv4 0 opts
      SockTypeTCP IPv6   -> sysSocket' SockRawTypeStream SockProtIPv6 0 opts
      SockTypeUDP IPv4   -> sysSocket' SockRawTypeDatagram SockProtIPv4 0 opts
      SockTypeUDP IPv6   -> sysSocket' SockRawTypeDatagram SockProtIPv6 0 opts
      SockTypeNetlink nt -> sysSocket' SockRawTypeDatagram SockProtNETLINK (fromEnum nt) opts

-- | Create a socket pair
sysSocketPair :: MonadInIO m => SocketType -> [SocketOption] -> Flow m '[(Handle,Handle),ErrorCode]
sysSocketPair typ opts =
   case typ of
      SockTypeTCP IPv4   -> sysSocketPair' SockRawTypeStream SockProtIPv4 0 opts
      SockTypeTCP IPv6   -> sysSocketPair' SockRawTypeStream SockProtIPv6 0 opts
      SockTypeUDP IPv4   -> sysSocketPair' SockRawTypeDatagram SockProtIPv4 0 opts
      SockTypeUDP IPv6   -> sysSocketPair' SockRawTypeDatagram SockProtIPv6 0 opts
      SockTypeNetlink nt -> sysSocketPair' SockRawTypeDatagram SockProtNETLINK (fromEnum nt) opts

-- | Bind a socket
sysBind :: (MonadInIO m, Storable a) => Handle -> a -> Flow m '[(),ErrorCode]
sysBind (Handle fd) addr =
   with addr $ \addr' ->
      liftIO (syscall_bind fd (castPtr addr') (sizeOf' addr))
         ||> toErrorCodeVoid

-- | Connect a socket
sysConnect :: (MonadInIO m, Storable a) => Handle -> a -> Flow m '[(),ErrorCode]
sysConnect (Handle fd) addr =
   with addr $ \addr' ->
      liftIO (syscall_connect fd (castPtr addr') (sizeOf' addr))
         ||> toErrorCodeVoid

-- | Accept a connection on a socket
--
-- We use accept4 (288) instead of accept (43) to support socket options
--
sysAccept :: (MonadInIO m, Storable a) => Handle -> a -> [SocketOption] -> Flow m '[Handle,ErrorCode]
sysAccept (Handle fd) addr opts =
   let 
      f :: Enum a => a -> Word64
      f = fromIntegral . fromEnum
      opts' = foldl' (\x y -> x .|. f y) 0 opts
   in
   with addr $ \addr' ->
      liftIO (syscall_accept4 fd (castPtr addr') (sizeOf' addr) opts')
         ||> toErrorCodePure (Handle . fromIntegral)

-- | Listen on a socket
--
-- @ backlog is the number of incoming requests that are stored
sysListen :: MonadIO m => Handle -> Word64 -> Flow m '[(),ErrorCode]
sysListen (Handle fd) backlog =
   liftIO (syscall_listen fd backlog)
      ||> toErrorCodeVoid


-- | Netlink socket binding
data NetlinkSocket
   = NetlinkSocket Word32 Word32 Word32
   deriving (Generic, Storable)

-- | Bind a netlink socket
--
-- @groups@ is a group mask
sysBindNetlink :: MonadInIO m => Handle -> Word32 -> Word32 -> Flow m '[(),ErrorCode]
sysBindNetlink fd portID groups = sysBind fd s
   where
      s = NetlinkSocket p portID groups
      p = fromIntegral (fromEnum SockProtNETLINK)
