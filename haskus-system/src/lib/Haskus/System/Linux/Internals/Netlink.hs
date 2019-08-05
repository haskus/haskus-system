{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- | Netlink generic interface
module Haskus.System.Linux.Internals.Netlink
   ( NetlinkType (..)
   , NetlinkSocket (..)
   , NetlinkMessageHeader (..)
   , NetlinkCommonFlag (..)
   , NetlinkCommonFlags
   , NetlinkGetFlag (..)
   , NetlinkGetFlags
   , NetlinkNewFlag (..)
   , NetlinkNewFlags
   , NetlinkDeleteFlag (..)
   , NetlinkDeleteFlags
   , NetlinkAckFlag (..)
   , NetlinkAckFlags
   , NetlinkMessageType (..)
   , NetlinkErrorMessageHeader (..)
   , NetlinkErrorAttribute (..)
   , NetlinkSocketOptions (..)
   , NetlinkPacketInfo (..)
   , NetlinkMmapRequest (..)
   , NetlinkMmapHeader (..)
   , NetlinkMmapStatus (..)
   , NetlinkConnectionState (..)
   , NetlinkAttributeHeader (..)
   , NetlinkAttributeBits (..)
   )
where

import Haskus.Utils.Types.Generics (Generic)
import Haskus.Utils.Flow
import Haskus.Binary.Storable
import Haskus.Number.Word
import Haskus.Binary.Enum
import Haskus.Binary.BitSet
import Haskus.Binary.BitField

-- =============================================================
--    From linux/include/uapi/linux/netlink.h
-- =============================================================

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
   | NetlinkTypeSMC              -- ^ SMC monitoring
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
      22 -> NetlinkTypeSMC
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
      NetlinkTypeSMC                -> 22

-- | Netlink socket address
--
-- sockaddr_nl
data NetlinkSocket = NetlinkSocket
   { netlinkSocketFamily :: Word32 -- ^ SockProtNETLINK (AF_NETLINK)
   , netlinkSocketPortID :: Word32 -- ^ Port ID
   , netlinkSocketGroups :: Word32 -- ^ Multicast groups mask
   }
   deriving (Generic, Storable)

-- | Netlink message header
--
-- nlmsghdr
data NetlinkMessageHeader = NetlinkMessageHeader
   { netlinkMessageLength   :: !Word32 -- ^ Length of message, including header
   , netlinkMessageType     :: !Word16 -- ^ Message content type
   , netlinkMessageFlags    :: !Word16 -- ^ Additional flags
   , netlinkMessageSequence :: !Word32 -- ^ Sequence number
   , netlinkMessagePortID   :: !Word32 -- ^ Sending process port ID
   }
   deriving (Generic, Storable)

-- | Netlink standard flags
data NetlinkCommonFlag
   = NetlinkFlagRequest          -- ^ Must be set on all request messages.
   | NetlinkFlagMultipart        -- ^ Multipart message, terminated by NLMSG_DONE
   | NetlinkFlagAck              -- ^ Request for an acknowledgment on success.
   | NetlinkFlagEcho             -- ^ Echo this request
   | NetlinkFlagDumpInconsistent -- ^ Dump was inconsistent due to sequence change
   | NetlinkFlagDumpFiltered     -- ^ Dump was filtered as requested
   deriving (Eq,Show,Enum,CEnum)

type NetlinkCommonFlags = BitSet Word16 NetlinkCommonFlag

-- | Additional flags for GET requests
data NetlinkGetFlag
   = NetlinkGetFlagFullTable -- ^ Return the complete table instead of a single entry.
   | NetlinkGetFlagMatch     -- ^ Return all entries matching criteria passed in message content. Not implemented yet.
   | NetlinkGetFlagAtomic    -- ^ Return an atomic snapshot of the table.
   -- | NetlinkGetFlagDump -- Root + Match flags...
   deriving (Show,Eq,Enum)

instance CEnum NetlinkGetFlag where
   toCEnum e   = toEnum (fromIntegral e - 8)
   fromCEnum e = fromIntegral <| fromEnum e + 8

type NetlinkGetFlags = BitSet Word16 NetlinkGetFlag

-- | Modifiers to NEW request
data NetlinkNewFlag
   = NetlinkNewFlagReplace   -- ^ Override existing
   | NetlinkNewFlagExclusive -- ^ Do not touch, if it exists
   | NetlinkNewFlagCreate    -- ^ Create, if it does not exist
   | NetlinkNewFlagAppend    -- ^ Add to end of list
   deriving (Show,Eq,Enum)

instance CEnum NetlinkNewFlag where
   toCEnum e   = toEnum (fromIntegral e - 8)
   fromCEnum e = fromIntegral <| fromEnum e + 8

type NetlinkNewFlags = BitSet Word16 NetlinkNewFlag

-- | Modifiers to DELETE request
data NetlinkDeleteFlag
   = NetlinkDeleteFlagNonRecursive -- ^ Do not delete recursively
   deriving (Show,Eq,Enum)

instance CEnum NetlinkDeleteFlag where
   toCEnum e   = toEnum (fromIntegral e - 8)
   fromCEnum e = fromIntegral <| fromEnum e + 8

type NetlinkDeleteFlags = BitSet Word16 NetlinkDeleteFlag

-- | Flags for ACK message
data NetlinkAckFlag
   = NetlinkAckFlagCapped       -- ^ Request was capped
   | NetlinkAckFlagExtendedTVLs -- ^ Extended ACK TVLs were included
   deriving (Show,Eq,Enum)

instance CEnum NetlinkAckFlag where
   toCEnum e   = toEnum (fromIntegral e - 8)
   fromCEnum e = fromIntegral <| fromEnum e + 8

type NetlinkAckFlags = BitSet Word16 NetlinkAckFlag


-- skipped: NLMSG macros (use functions instead)



-- | Netlink message types.
--
-- Values < 0x10 are reserved control messages
data NetlinkMessageType
   = NetlinkMessageNoop    -- ^ Nothing
   | NetlinkMessageError   -- ^ Error
   | NetlinkMessageDone    -- ^ End of a dump
   | NetlinkMessageOverrun -- ^ Data lost
   deriving (Show,Eq,Enum)

instance CEnum NetlinkMessageType where
   toCEnum e   = toEnum (fromIntegral e - 1)
   fromCEnum e = fromIntegral <| fromEnum e + 1 

-- | Netlink error message header (beginning of the payload of a message with the NetlinkMessageError type).
--
-- The header is followed by the message contents unless NETLINK_CAP_ACK
-- was set or the ACK indicates success (error number == 0)
--
-- Then it is followed by TLVs defined by netlink error message attributes if NETLINK_EXT_ACK was set
data NetlinkErrorMessageHeader = NetlinkErrorMessageHeader
   { netlinkErrorNumber :: Int -- ^ Negative errno or 0 for acknowledgements
   , netlinkErrorHeader :: NetlinkMessageHeader -- ^ Message header that caused the error
   }
   deriving (Generic,Storable)

-- | Netlink error message attributes
data NetlinkErrorAttribute
   = NetlinkErrorAttributeUnused       -- ^ Unused
   | NetlinkErrorAttributeErrorMessage -- ^ Error message string (C string)
   | NetlinkErrorAttributeInvalidAttributeOffset -- ^ Offset of the invalid attribute in the original message, counting from the beginning of the header (Word32)
   | NetlinkErrorAttributeCookie -- ^ Arbitrary subsystem specific cookie to be used - in the success case - to identify a created object or operation or similar (binary)
   deriving (Show,Eq,Enum,CEnum)

-- | Netlink socket options
data NetlinkSocketOptions
   = NetlinkOptionAddMembership
   | NetlinkOptionDropMembership
   | NetlinkOptionPacketInfo
   | NetlinkOptionBroadcastError
   | NetlinkOptionNoBufferingError
   | NetlinkOptionRxRing
   | NetlinkOptionTxRing
   | NetlinkOptionListenAllNSID
   | NetlinkOptionListMemberships
   | NetlinkOptionCapAck
   | NetlinkOptionExtendedAck
   deriving (Show,Eq,Enum)

instance CEnum NetlinkSocketOptions where
   toCEnum e   = toEnum (fromIntegral e - 1)
   fromCEnum e = fromIntegral <| fromEnum e + 1

-- | Netlink packet info
--
-- nl_pktinfo
data NetlinkPacketInfo = NetlinkPacketInfo
   { netlinkPacketGroup :: !Word32
   }
   deriving (Generic,Storable)

-- | Netlink MMAP request
--
-- nl_mmap_req
data NetlinkMmapRequest = NetlinkMmapRequest
   { netlinkMmapBlockSize   :: !Word
   , netlinkMmapBlockNumber :: !Word
   , netlinkMmapFrameSize   :: !Word
   , netlinkMmapFrameNumber :: !Word
   }
   deriving (Generic,Storable)

-- | Netlink MMAP header
data NetlinkMmapHeader = NetlinkMmapHeader
   { netlinkMmapStatus :: !Word
   , netlinkMmapLength :: !Word
   , netlinkMmapGroup  :: !Word32
   , netlinkMmapPID    :: !Word32
   , netlinkMmapUID    :: !Word32
   , netlinkMmapGID    :: !Word32
   }
   deriving (Generic,Storable)

-- | Netlink MMAP status
data NetlinkMmapStatus
   = NetlinkMmapUnused
   | NetlinkMmapReserved
   | NetlinkMmapValid
   | NetlinkMmapCopy
   | NetlinkMmapSkip
   deriving (Show,Eq,Enum,CEnum)


-- | Connection state
data NetlinkConnectionState
   = NetlinkUnconnected
   | NetlinkConnected
   deriving (Show,Eq,Enum,CEnum)


--------------------
-- Attributes
--------------------

-- | Netlink attribute header
--
-- Followed by payload (+ 4-byte alignment padding).
--
-- Attribute length includes header length (4 bytes) but doesn't include
-- final padding bytes.
--
-- "Nested" and "NetworkByteOrder" flags are mutually exclusive.
--
-- struct nlattr
data NetlinkAttributeHeader = NetlinkAttributeHeader
   { netlinkAttributeLength :: !Word16
   , netlinkAttributeType   :: BitFields Word16 '[ BitField 1  "Nested" Bool -- carries nested attributes
                                                 , BitField 1  "NetworkByteOrder" Bool -- payload stored in network byte order
                                                 , BitField 14 "AttributeType" Word16 -- attribute type
                                                 ]
   }


-- | Generic 32 bitflags attribute content sent to the kernel.
--                                                                    
-- The value is a bitmap that defines the values being set
-- The mask is a bitmask that defines which value is legit
--                                                                    
-- Examples:
--  value = 0x0, and mask = 0x1
--  implies we are selecting bit 1 and we want to set its value to 0.
--                                                                    
--  value = 0x2, and mask = 0x2
--  implies we are selecting bit 2 and we want to set its value to 1.
data NetlinkAttributeBits = NetlinkAttributeBits
   { netlinkAttributeValues :: !Word32
   , netlinkAttributeMask   :: !Word32
   }
