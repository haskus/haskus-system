{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Haskus.System.Linux.Internals.IfLink where

import Haskus.Utils.Types.Generics (Generic)
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Vector (Vector)
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.BitSet
import Haskus.Format.Binary.Endianness

-- =============================================================
--    From linux/include/uapi/linux/if_link.h
-- =============================================================


-- This struct should be in sync with struct rtnl_link_stats64
data LinkStats = LinkStats
   { linkStatsRxPackets  :: !Word32 -- ^ total packets received
   , linkStatsTxPackets  :: !Word32 -- ^ total packets transmitted
   , linkStatsRxBytes    :: !Word32 -- ^ total bytes received
   , linkStatsTxBytes    :: !Word32 -- ^ total bytes transmitted
   , linkStatsRxErrors   :: !Word32 -- ^ bad packets received
   , linkStatsTxErrors   :: !Word32 -- ^ packet transmit problems
   , linkStatsRxDropped  :: !Word32 -- ^ no space in linux buffers
   , linkStatsTxDropped  :: !Word32 -- ^ no space available in linux
   , linkStatsMulticast  :: !Word32 -- ^ multicast packets received
   , linkStatsCollisions :: !Word32

   -- detailed rx_errors:
   , linkStatsRxLengthErrors :: !Word32
   , linkStatsRxOverErrors   :: !Word32 -- ^ receiver ring buff overflow
   , linkStatsRxCrcErrors    :: !Word32 -- ^ recved pkt with crc error
   , linkStatsRxFrameErrors  :: !Word32 -- ^ recv'd frame alignment error
   , linkStatsRxFifoErrors   :: !Word32 -- ^ recv'r fifo overrun
   , linkStatsRxMissedErrors :: !Word32 -- ^ receiver missed packet

   -- detailed tx_errors
   , linkStatsTxAbortedErrors   :: !Word32
   , linkStatsTxCarrierErrors   :: !Word32
   , linkStatsTxFifoErrors      :: !Word32
   , linkStatsTxHeartbeatErrors :: !Word32
   , linkStatsTxWindowErrors    :: !Word32

   -- for cslip etc
   , linkStatsRxCompressed :: !Word32
   , linkStatsTxCompressed :: !Word32

   , linkStatsRxNohandler  :: !Word32 -- ^ dropped, no handler found
   }
   deriving (Generic,Storable,Show)

-- | The main device statistics structure
data LinkStats64 = LinkStats64
   { linkStats64RxPackets  :: !Word64 -- ^ total packets received
   , linkStats64TxPackets  :: !Word64 -- ^ total packets transmitted
   , linkStats64RxBytes    :: !Word64 -- ^ total bytes received
   , linkStats64TxBytes    :: !Word64 -- ^ total bytes transmitted
   , linkStats64RxErrors   :: !Word64 -- ^ bad packets received
   , linkStats64TxErrors   :: !Word64 -- ^ packet transmit problems
   , linkStats64RxDropped  :: !Word64 -- ^ no space in linux buffers
   , linkStats64TxDropped  :: !Word64 -- ^ no space available in linux
   , linkStats64Multicast  :: !Word64 -- ^ multicast packets received
   , linkStats64Collisions :: !Word64

   -- detailed rx_errors:
   , linkStats64RxLengthErrors :: !Word64
   , linkStats64RxOverErrors   :: !Word64 -- ^ receiver ring buff overflow
   , linkStats64RxCrcErrors    :: !Word64 -- ^ recved pkt with crc error
   , linkStats64RxFrameErrors  :: !Word64 -- ^ recv'd frame alignment error
   , linkStats64RxFifoErrors   :: !Word64 -- ^ recv'r fifo overrun
   , linkStats64RxMissedErrors :: !Word64 -- ^ receiver missed packet

   -- detailed tx_errors
   , linkStats64TxAbortedErrors   :: !Word64
   , linkStats64TxCarrierErrors   :: !Word64
   , linkStats64TxFifoErrors      :: !Word64
   , linkStats64TxHeartbeatErrors :: !Word64
   , linkStats64TxWindowErrors    :: !Word64

   -- for cslip etc
   , linkStats64RxCompressed :: !Word64
   , linkStats64TxCompressed :: !Word64

   , linkStats64RxNohandler  :: !Word64 -- ^ dropped, no handler found
   }
   deriving (Generic,Storable,Show)

-- | The struct should be in sync with struct ifmap
data LinkIfMap = LinkIfMap
   { linkIfMapMemStart :: !Word64
   , linkIfMapMemEnd   :: !Word64
   , linkIfMapBaseAddr :: !Word64
   , linkIfMapIRQ      :: !Word16
   , linkIfMapDMA      :: !Word8
   , linkIfMapPort     :: !Word8
   }
   deriving (Generic,Storable,Show)

--
-- IFLA_AF_SPEC
--   Contains nested attributes for address family specific attributes.
--   Each address family may create a attribute with the address family
--   number as type and create its own attribute structure in it.
--
--   Example:
--   [IFLA_AF_SPEC] = {
--       [AF_INET] = {
--           [IFLA_INET_CONF] = ...,
--       },
--       [AF_INET6] = {
--           [IFLA_INET6_FLAGS] = ...,
--           [IFLA_INET6_CONF] = ...,
--       }
--   }
--

data InterfaceFlag
   = IflaUNSPEC
   | IflaADDRESS
   | IflaBROADCAST
   | IflaIFNAME
   | IflaMTU
   | IflaLINK
   | IflaQDISC
   | IflaSTATS
   | IflaCOST
   | IflaPRIORITY
   | IflaMASTER
   | IflaWIRELESS -- ^ Wireless Extension event - see wireless.h
   | IflaPROTINFO -- ^ Protocol specific information for a link
   | IflaTXQLEN
   | IflaMAP
   | IflaWEIGHT
   | IflaOPERSTATE
   | IflaLINKMODE
   | IflaLINKINFO
   | IflaNET_NS_PID
   | IflaIFALIAS
   | IflaNUM_VF   -- ^ Number of VFs if device is SR-IOV PF
   | IflaVFINFO_LIST
   | IflaSTATS64
   | IflaVF_PORTS
   | IflaPORT_SELF
   | IflaAF_SPEC
   | IflaGROUP       -- ^ Group the device belongs to
   | IflaNET_NS_FD
   | IflaEXT_MASK    -- ^ Extended info mask, VFs, etc.
   | IflaPROMISCUITY -- ^ Promiscuity count: > 0 means acts PROMISC
   | IflaNUM_TX_QUEUES
   | IflaNUM_RX_QUEUES
   | IflaCARRIER
   | IflaPHYS_PORT_ID
   | IflaCARRIER_CHANGES
   | IflaPHYS_SWITCH_ID
   | IflaLINK_NETNSID
   | IflaPHYS_PORT_NAME
   | IflaPROTO_DOWN
   | IflaGSO_MAX_SEGS
   | IflaGSO_MAX_SIZE
   | IflaPAD
   | IflaXDP
   deriving (Show,Eq,Enum)


data InterfaceFlagInet
   = IflaInetUnspec
   | IflaInetConf
   deriving (Show,Eq,Enum)


-- ifi_flags.
--
-- IFF_* flags.
--
-- The only change is:
-- IFF_LOOPBACK, IFF_BROADCAST and IFF_POINTOPOINT are
-- more not changeable by user. They describe link media
-- characteristics and set by device driver.
--
-- Comments:
-- - Combination IFF_BROADCAST|IFF_POINTOPOINT is invalid
-- - If neither of these three flags are set;
--   the interface is NBMA.
--
-- - IFF_MULTICAST does not mean anything special:
-- multicasts can be used on all not-NBMA links.
-- IFF_MULTICAST means that this media uses special encapsulation
-- for multicast frames. Apparently, all IFF_POINTOPOINT and
-- IFF_BROADCAST devices are able to use multicasts too.
--

-- IFLA_LINK.
-- For usual devices it is equal ifi_index.
-- If it is a "virtual interface" (f.e. tunnel), ifi_link
-- can point to real physical interface (f.e. for bandwidth calculations),
-- or maybe 0, what means, that real media is unknown (usual
-- for IPIP tunnels, when route to endpoint is allowed to change)
--

-- | Subtype attributes for IFLA_PROTINFO
data IflaInet6
   = IflaInet6UNSPEC
   | IflaInet6FLAGS         -- ^ link flags
   | IflaInet6CONF          -- ^ sysctl parameters
   | IflaInet6STATS         -- ^ statistics
   | IflaInet6MCAST         -- ^ MC things. What of them?
   | IflaInet6CACHEINFO     -- ^ time values and max reasm size
   | IflaInet6ICMP6STATS    -- ^ statistics (icmpv6)
   | IflaInet6TOKEN         -- ^ device token
   | IflaInet6ADDR_GEN_MODE -- ^ implicit address generator mode
   deriving (Show,Eq,Enum)

data In6AddrGenMode
   = In6AddrGenModeEUI64
   | In6AddrGenModeNone
   | In6AddrGenModeStablePrivacy
   | In6AddrGenModeRandom
   deriving (Show,Eq,Enum)

--------------------
-- Bridge section
--------------------

data IflaBridge
  = IFLA_BR_UNSPEC
  | IFLA_BR_FORWARD_DELAY
  | IFLA_BR_HELLO_TIME
  | IFLA_BR_MAX_AGE
  | IFLA_BR_AGEING_TIME
  | IFLA_BR_STP_STATE
  | IFLA_BR_PRIORITY
  | IFLA_BR_VLAN_FILTERING
  | IFLA_BR_VLAN_PROTOCOL
  | IFLA_BR_GROUP_FWD_MASK
  | IFLA_BR_ROOT_ID
  | IFLA_BR_BRIDGE_ID
  | IFLA_BR_ROOT_PORT
  | IFLA_BR_ROOT_PATH_COST
  | IFLA_BR_TOPOLOGY_CHANGE
  | IFLA_BR_TOPOLOGY_CHANGE_DETECTED
  | IFLA_BR_HELLO_TIMER
  | IFLA_BR_TCN_TIMER
  | IFLA_BR_TOPOLOGY_CHANGE_TIMER
  | IFLA_BR_GC_TIMER
  | IFLA_BR_GROUP_ADDR
  | IFLA_BR_FDB_FLUSH
  | IFLA_BR_MCAST_ROUTER
  | IFLA_BR_MCAST_SNOOPING
  | IFLA_BR_MCAST_QUERY_USE_IFADDR
  | IFLA_BR_MCAST_QUERIER
  | IFLA_BR_MCAST_HASH_ELASTICITY
  | IFLA_BR_MCAST_HASH_MAX
  | IFLA_BR_MCAST_LAST_MEMBER_CNT
  | IFLA_BR_MCAST_STARTUP_QUERY_CNT
  | IFLA_BR_MCAST_LAST_MEMBER_INTVL
  | IFLA_BR_MCAST_MEMBERSHIP_INTVL
  | IFLA_BR_MCAST_QUERIER_INTVL
  | IFLA_BR_MCAST_QUERY_INTVL
  | IFLA_BR_MCAST_QUERY_RESPONSE_INTVL
  | IFLA_BR_MCAST_STARTUP_QUERY_INTVL
  | IFLA_BR_NF_CALL_IPTABLES
  | IFLA_BR_NF_CALL_IP6TABLES
  | IFLA_BR_NF_CALL_ARPTABLES
  | IFLA_BR_VLAN_DEFAULT_PVID
  | IFLA_BR_PAD
  | IFLA_BR_VLAN_STATS_ENABLED
  | IFLA_BR_MCAST_STATS_ENABLED
  | IFLA_BR_MCAST_IGMP_VERSION
  | IFLA_BR_MCAST_MLD_VERSION
   deriving (Show,Eq,Enum)


data BridgeId = BridgeId
   { bridgePriority :: Vector 2 Word8
   , bridgeAddress  :: Vector 6 Word8 -- ETH_ALEN
   }
   deriving (Generic,Storable,Show)

data BridgeMode
   = BridgeModeUnspec
   | BridgeModeHairpin
   deriving (Show,Eq,Enum)

data BridgePort
   = BridgePortUNSPEC
   | BridgePortSTATE          -- ^ Spanning tree state
   | BridgePortPRIORITY       -- ^ Spanning tree priority
   | BridgePortCOST           -- ^ Spanning tree cost
   | BridgePortMODE           -- ^ mode (hairpin)
   | BridgePortGUARD          -- ^ bpdu guard
   | BridgePortPROTECT        -- ^ root port protection
   | BridgePortFAST_LEAVE     -- ^ multicast fast leave
   | BridgePortLEARNING       -- ^ mac learning
   | BridgePortUNICAST_FLOOD  -- ^ flood unicast traffic
   | BridgePortPROXYARP       -- ^ proxy ARP
   | BridgePortLEARNING_SYNC  -- ^ mac learning sync from device
   | BridgePortPROXYARP_WIFI  -- ^ proxy ARP for Wi-Fi
   | BridgePortROOT_ID        -- ^ designated root
   | BridgePortBRIDGE_ID      -- ^ designated bridge
   | BridgePortDESIGNATED_PORT
   | BridgePortDESIGNATED_COST
   | BridgePortID
   | BridgePortNO
   | BridgePortTOPOLOGY_CHANGE_ACK
   | BridgePortCONFIG_PENDING
   | BridgePortMESSAGE_AGE_TIMER
   | BridgePortFORWARD_DELAY_TIMER
   | BridgePortHOLD_TIMER
   | BridgePortFLUSH
   | BridgePortMULTICAST_ROUTER
   | BridgePortPAD
   | BridgePortMCAST_FLOOD
   | BridgePortMCAST_TO_UCAST
   | BridgePortVLAN_TUNNEL
   | BridgePortBCAST_FLOOD
   deriving (Show,Eq,Enum)

data CacheInfo = CacheInfo
   { cacheInfoMaxReasmLen   :: Word32
   , cacheInfoTimestamp     :: Word32   -- ^ ipv6InterfaceTable updated timestamp
   , cacheInfoReachableTime :: Word32
   , cacheInfoRetransTime   :: Word32
   }
   deriving (Generic,Storable,Show)

data InterfaceInfo
   = InterfaceInfoUNSPEC
   | InterfaceInfoKIND
   | InterfaceInfoDATA
   | InterfaceInfoXSTATS
   | InterfaceInfoSLAVE_KIND
   | InterfaceInfoSLAVE_DATA
   deriving (Show,Eq,Enum)


-----------------
-- VLAN section
-----------------

data VLAN
   = VLAN_UNSPEC
   | VLAN_ID
   | VLAN_FLAGS
   | VLAN_EGRESS_QOS
   | VLAN_INGRESS_QOS
   | VLAN_PROTOCOL
   deriving (Show,Eq,Enum)


data VLANFlags = VLANFlags
   { vlanFlags :: !Word32
   , vlanMask  :: !Word32
   }
   deriving (Generic,Storable,Show)

data VLAN_QOS
   = VLAN_QOS_UNSPEC
   | VLAN_QOS_MAPPING
   deriving (Show,Eq,Enum)

data VLAN_QOS_Mapping = VLAN_QOS_Mapping
   { vlanQosMappingFrom :: !Word32
   , vlanQosMappingTo   :: !Word32
   }
   deriving (Generic,Storable,Show)

--------------------
-- MACVLAN section 
--------------------

data MACVLAN
   = MACVLAN_UNSPEC
   | MACVLAN_MODE
   | MACVLAN_FLAGS
   | MACVLAN_MACADDR_MODE
   | MACVLAN_MACADDR
   | MACVLAN_MACADDR_DATA
   | MACVLAN_MACADDR_COUNT
   deriving (Show,Eq,Enum)

data MACVLAN_Mode
   = MACVLAN_MODE_PRIVATE  -- ^ don't talk to other macvlans
   | MACVLAN_MODE_VEPA     -- ^ talk to other ports through ext bridge
   | MACVLAN_MODE_BRIDGE   -- ^ talk to bridge ports directly
   | MACVLAN_MODE_PASSTHRU -- ^ take over the underlying device
   | MACVLAN_MODE_SOURCE   -- ^ use source MAC address list to assign
   deriving (Show,Eq,Enum)

type MACVLAN_Modes = BitSet Word32 MACVLAN_Mode

data MacAddrMode
   = MACADDR_ADD
   | MACADDR_DEL
   | MACADDR_FLUSH
   | MACADDR_SET
   deriving (Show,Eq,Enum)

-- #define MACVLAN_FLAG_NOPROMISC 1

----------------
-- VRF section
----------------

data VRF
   = VRF_UNSPEC
   | VRF_TABLE
   deriving (Show,Eq,Enum)


data VRF_Port
   = VRF_PORT_UNSPEC
   | VRF_PORT_TABLE
   deriving (Show,Eq,Enum)


------------------
-- MACSEC section
------------------

data MACSEC
   = MACSEC_UNSPEC
   | MACSEC_SCI
   | MACSEC_PORT
   | MACSEC_ICV_LEN
   | MACSEC_CIPHER_SUITE
   | MACSEC_WINDOW
   | MACSEC_ENCODING_SA
   | MACSEC_ENCRYPT
   | MACSEC_PROTECT
   | MACSEC_INC_SCI
   | MACSEC_ES
   | MACSEC_SCB
   | MACSEC_REPLAY_PROTECT
   | MACSEC_VALIDATION
   | MACSEC_PAD
   deriving (Show,Eq,Enum)

data MACSEC_ValidationType
   = MACSEC_VALIDATE_DISABLED
   | MACSEC_VALIDATE_CHECK
   | MACSEC_VALIDATE_STRICT
   | MACSEC_VALIDATE_END
   deriving (Show,Eq,Enum)

-------------------
-- IPVLAN section
-------------------


data IPVLAN
   = IPVLAN_UNSPEC
   | IPVLAN_MODE
   deriving (Show,Eq,Enum)

data IPVLAN_Mode
   = IPVLAN_MODE_L2
   | IPVLAN_MODE_L3
   | IPVLAN_MODE_L3S
   deriving (Show,Eq,Enum)

-------------------
-- VXLAN section
-------------------

data VXLAN
   = VXLAN_UNSPEC
   | VXLAN_ID
   | VXLAN_GROUP -- ^ group or remote address
   | VXLAN_LINK
   | VXLAN_LOCAL
   | VXLAN_TTL
   | VXLAN_TOS
   | VXLAN_LEARNING
   | VXLAN_AGEING
   | VXLAN_LIMIT
   | VXLAN_PORT_RANGE -- ^ source port
   | VXLAN_PROXY
   | VXLAN_RSC
   | VXLAN_L2MISS
   | VXLAN_L3MISS
   | VXLAN_PORT -- ^ destination port
   | VXLAN_GROUP6
   | VXLAN_LOCAL6
   | VXLAN_UDP_CSUM
   | VXLAN_UDP_ZERO_CSUM6_TX
   | VXLAN_UDP_ZERO_CSUM6_RX
   | VXLAN_REMCSUM_TX
   | VXLAN_REMCSUM_RX
   | VXLAN_GBP
   | VXLAN_REMCSUM_NOPARTIAL
   | VXLAN_COLLECT_METADATA
   | VXLAN_LABEL
   | VXLAN_GPE
   deriving (Show,Eq,Enum)


data VXLAN_PortRange = VXLAN_PortRange
   { vxlanPortRangeLow  :: AsBigEndian Word16
   , vxlanPortRangeHigh :: AsBigEndian Word16
   }
   deriving (Generic,Storable,Show)

-------------------
-- GENEVE section
-------------------

data Geneve
   = GENEVE_UNSPEC
   | GENEVE_ID
   | GENEVE_REMOTE
   | GENEVE_TTL
   | GENEVE_TOS
   | GENEVE_PORT -- ^ destination port
   | GENEVE_COLLECT_METADATA
   | GENEVE_REMOTE6
   | GENEVE_UDP_CSUM
   | GENEVE_UDP_ZERO_CSUM6_TX
   | GENEVE_UDP_ZERO_CSUM6_RX
   | GENEVE_LABEL
   deriving (Show,Eq,Enum)

----------------
-- PPP section
----------------

data PPP
   = PPP_UNSPEC
   | PPP_DEV_FD
   deriving (Show,Eq,Enum)

----------------
-- GTP section
----------------

data GTP_Role
   = GTP_ROLE_GGSN
   | GTP_ROLE_SGSN
   deriving (Show,Eq,Enum)

data GTP
   = GTP_UNSPEC
   | GTP_FD0
   | GTP_FD1
   | GTP_PDP_HASHSIZE
   | GTP_ROLE
   deriving (Show,Eq,Enum)

--------------------
-- Bonding section
--------------------

data Bond
   = BOND_UNSPEC
   | BOND_MODE
   | BOND_ACTIVE_SLAVE
   | BOND_MIIMON
   | BOND_UPDELAY
   | BOND_DOWNDELAY
   | BOND_USE_CARRIER
   | BOND_ARP_INTERVAL
   | BOND_ARP_IP_TARGET
   | BOND_ARP_VALIDATE
   | BOND_ARP_ALL_TARGETS
   | BOND_PRIMARY
   | BOND_PRIMARY_RESELECT
   | BOND_FAIL_OVER_MAC
   | BOND_XMIT_HASH_POLICY
   | BOND_RESEND_IGMP
   | BOND_NUM_PEER_NOTIF
   | BOND_ALL_SLAVES_ACTIVE
   | BOND_MIN_LINKS
   | BOND_LP_INTERVAL
   | BOND_PACKETS_PER_SLAVE
   | BOND_AD_LACP_RATE
   | BOND_AD_SELECT
   | BOND_AD_INFO
   | BOND_AD_ACTOR_SYS_PRIO
   | BOND_AD_USER_PORT_KEY
   | BOND_AD_ACTOR_SYSTEM
   | BOND_TLB_DYNAMIC_LB
   deriving (Show,Eq,Enum)


data BondAdInfo
   = BOND_AD_INFO_UNSPEC
   | BOND_AD_INFO_AGGREGATOR
   | BOND_AD_INFO_NUM_PORTS
   | BOND_AD_INFO_ACTOR_KEY
   | BOND_AD_INFO_PARTNER_KEY
   | BOND_AD_INFO_PARTNER_MAC
   deriving (Show,Eq,Enum)

data BondSlave
   = BOND_SLAVE_UNSPEC
   | BOND_SLAVE_STATE
   | BOND_SLAVE_MII_STATUS
   | BOND_SLAVE_LINK_FAILURE_COUNT
   | BOND_SLAVE_PERM_HWADDR
   | BOND_SLAVE_QUEUE_ID
   | BOND_SLAVE_AD_AGGREGATOR_ID
   | BOND_SLAVE_AD_ACTOR_OPER_PORT_STATE
   | BOND_SLAVE_AD_PARTNER_OPER_PORT_STATE
   deriving (Show,Eq,Enum)


----------------------------------------------
-- SR-IOV virtual function management section
----------------------------------------------

data VF_Info
   = VF_INFO_UNSPEC
   | VF_INFO
   deriving (Show,Eq,Enum)

data VF_TYPE
   = VF_TYPE_UNSPEC
   | VF_TYPE_MAC          -- ^ Hardware queue specific attributes
   | VF_TYPE_VLAN         -- ^ VLAN ID and QoS
   | VF_TYPE_TX_RATE      -- ^ Max TX Bandwidth Allocation
   | VF_TYPE_SPOOFCHK     -- ^ Spoof Checking on/off switch
   | VF_TYPE_LINK_STATE   -- ^ link state enable/disable/auto switch
   | VF_TYPE_RATE         -- ^ Min and Max TX Bandwidth Allocation
   | VF_TYPE_RSS_QUERY_EN -- ^ RSS Redirection Table and Hash Key query
                          -- on/off switch
   | VF_TYPE_STATS        -- ^ network device statistics
   | VF_TYPE_TRUST        -- ^ Trust VF
   | VF_TYPE_IB_NODE_GUID -- ^ VF Infiniband node GUID
   | VF_TYPE_IB_PORT_GUID -- ^ VF Infiniband port GUID
   | VF_TYPE_VLAN_LIST    -- ^ nested list of vlans, option for QinQ
   deriving (Show,Eq,Enum)

data VF_MAC = VF_MAC
   { vfMacVF  :: !Word32
   , vfMacMac :: Vector 32 Word8 -- MAX_ADDR_LEN
   }
   deriving (Generic,Storable,Show)

data VF_VLAN = VF_VLAN
   { vfVlanVF   :: !Word32
   , vfVlanVLAN :: !Word32 -- ^ 0 - 4095, 0 disables VLAN filter
   , vfVlanQOS  :: !Word32
   }
   deriving (Generic,Storable,Show)

data VF_VLAN_INFO
   = VF_VLAN_INFO_UNSPEC
   | VF_VLAN_INFO         -- ^ VLAN ID, QoS and VLAN protocol
   deriving (Show,Eq,Enum)

data VFVlanInfo = VFVlanInfo
   { vfVlanInfoVF   :: !Word32
   , vfVlanInfoVLAN :: !Word32 -- ^ 0 - 4095, 0 disables VLAN filter
   , vfVlanInfoQOS  :: !Word32
   , vfVlanProto    :: AsBigEndian Word16 -- ^ VLAN protocol either 802.1Q or 802.1ad
   }
   deriving (Generic,Storable,Show)

data VF_TX_RATE = VF_TX_RATE
   { vfTxRateVF   :: !Word32
   , vfTxRateRate :: !Word32 -- ^ Max TX bandwidth in Mbps, 0 disables throttling
   }
   deriving (Generic,Storable,Show)

data VF_RATE = VF_RATE
   { vfRateVF :: !Word32
   , vfRateMinTxRate :: !Word32 -- ^ Min Bandwidth in Mbps
   , vfRateMaxTxRate :: !Word32 -- ^ Max Bandwidth in Mbps
   }
   deriving (Generic,Storable,Show)

data VF_SpoofCheck = VF_SpoofCheck
   { vfSpoofCheckVF      :: !Word32
   , vfSpoofCheckSetting :: !Word32
   }
   deriving (Generic,Storable,Show)

data VF_GUID = VF_GUID
   { vfGuidVF   :: !Word32
   , vfGuidGUID :: !Word64
   }
   deriving (Generic,Storable,Show)

data VF_LINK_STATE
   = VF_LINK_STATE_AUTO    -- ^ link state of the uplink
   | VF_LINK_STATE_ENABLE -- ^ link always up
   | VF_LINK_STATE_DISABLE -- ^ link always down
   deriving (Show,Eq,Enum)

data VFLinkState = VFLinkState
   { vfLinkStateVF    :: !Word32
   , vfLinkStateState :: !Word32
   }
   deriving (Generic,Storable,Show)

data VF_RSS_QUERY_EN = VF_RSS_QUERY_EN
   { vfRssQueryVF      :: !Word32
   , vfRssQuerySetting :: !Word32
   }
   deriving (Generic,Storable,Show)

data VF_STATS
   = VF_STATS_RX_PACKETS
   | VF_STATS_TX_PACKETS
   | VF_STATS_RX_BYTES
   | VF_STATS_TX_BYTES
   | VF_STATS_BROADCAST
   | VF_STATS_MULTICAST
   | VF_STATS_PAD
   deriving (Show,Eq,Enum)


data VF_Trust = VF_Trust
   { vfTrustVF      :: !Word32
   , vfTrustSetting :: !Word32
   }
   deriving (Generic,Storable,Show)

-- VF ports management section
--
--   Nested layout of set/get msg is:
--
--      [IFLA_NUM_VF]
--      [IFLA_VF_PORTS]
--         [IFLA_VF_PORT]
--            [IFLA_PORT_*], ...
--         [IFLA_VF_PORT]
--            [IFLA_PORT_*], ...
--         ...
--      [IFLA_PORT_SELF]
--         [IFLA_PORT_*], ...
--

data VF_PORT
   = VF_PORT_UNSPEC
   | VF_PORT         -- ^ nest
   deriving (Show,Eq,Enum)


data PORT
   = PORT_UNSPEC
   | PORT_VF            -- __u32
   | PORT_PROFILE       -- string
   | PORT_VSI_TYPE      -- 802.1Qbg (pre-)standard VDP
   | PORT_INSTANCE_UUID -- binary UUID
   | PORT_HOST_UUID     -- binary UUID
   | PORT_REQUEST       -- __u8
   | PORT_RESPONSE      -- __u16, output only
   deriving (Show,Eq,Enum)

-- #define PORT_PROFILE_MAX 40
-- #define PORT_UUID_MAX    16
-- #define PORT_SELF_VF     -1

data Request
   = REQUEST_PREASSOCIATE
   | REQUEST_PREASSOCIATE_RR
   | REQUEST_ASSOCIATE
   | REQUEST_DISASSOCIATE
   deriving (Show,Eq,Enum)

data PORT_VDP
   = PORT_VDP_RESPONSE_SUCCESS
   | PORT_VDP_RESPONSE_INVALID_FORMAT
   | PORT_VDP_RESPONSE_INSUFFICIENT_RESOURCES
   | PORT_VDP_RESPONSE_UNUSED_VTID
   | PORT_VDP_RESPONSE_VTID_VIOLATION
   | PORT_VDP_RESPONSE_VTID_VERSION_VIOALTION
   | PORT_VDP_RESPONSE_OUT_OF_SYNC
   -- 0x08-0xFF reserved for future VDP use (TODO)
   | PORT_PROFILE_RESPONSE_SUCCESS -- = 0x100
   | PORT_PROFILE_RESPONSE_INPROGRESS
   | PORT_PROFILE_RESPONSE_INVALID
   | PORT_PROFILE_RESPONSE_BADSTATE
   | PORT_PROFILE_RESPONSE_INSUFFICIENT_RESOURCES
   | PORT_PROFILE_RESPONSE_ERROR
   deriving (Show,Eq)

data PORT_VSI = PORT_VSI
   { vsiManagerId   :: !Word8
   , vsiTypeId      :: Vector 3 Word8
   , vsiTypeVersion :: !Word8
   , vsiPadding     :: Vector 3 Word8
   }
   deriving (Generic,Storable,Show)


-----------------
-- IPoIB section
-----------------

data IPOIB
   = IPOIB_UNSPEC
   | IPOIB_PKEY
   | IPOIB_MODE
   | IPOIB_UMCAST
   deriving (Show,Eq,Enum)

data IPOIB_MODE
   = IPOIB_MODE_DATAGRAM  -- ^ using unreliable datagram QPs
   | IPOIB_MODE_CONNECTED -- ^ using connected QPs
   deriving (Show,Eq,Enum)

----------------
-- HSR section
----------------

data HSR
   = HSR_UNSPEC
   | HSR_SLAVE1
   | HSR_SLAVE2
   | HSR_MULTICAST_SPEC   -- ^ Last byte of supervision addr
   | HSR_SUPERVISION_ADDR -- ^ Supervision frame multicast addr
   | HSR_SEQ_NR
   | HSR_VERSION          -- ^ HSR version
   deriving (Show,Eq,Enum)

-----------------
-- STATS section
-----------------

data InterfaceStatsMsg = InterfaceStatsMsg
   { ifStatsMsgFamily     :: !Word8
   , ifStatsMsgPad1       :: !Word8
   , ifStatsMsgPad2       :: !Word16
   , ifStatsMsgIfIndex    :: !Word32
   , ifStatsMsgFilterMask :: !Word32
   }
   deriving (Generic,Storable,Show)

-- A stats attribute can be netdev specific or a global stat.
-- For netdev stats, lets use the prefix IFLA_STATS_LINK_*

data STATS
   = STATS_UNSPEC   -- ^ also used as 64bit pad attribute
   | STATS_LINK_64
   | STATS_LINK_XSTATS
   | STATS_LINK_XSTATS_SLAVE
   | STATS_LINK_OFFLOAD_XSTATS
   | STATS_AF_SPEC
   deriving (Show,Eq,Enum)

-- #define IFLA_STATS_FILTER_BIT(ATTR) (1 << (ATTR - 1))

-- These are embedded into IFLA_STATS_LINK_XSTATS:
-- [IFLA_STATS_LINK_XSTATS]
-- -> [LINK_XSTATS_TYPE_xxx]
--    -> [rtnl link type specific attributes]

data LINK_XSTATS_TYPE
   = LINK_XSTATS_TYPE_UNSPEC
   | LINK_XSTATS_TYPE_BRIDGE
   deriving (Show,Eq,Enum)

-- These are stats embedded into IFLA_STATS_LINK_OFFLOAD_XSTATS

data OFFLOAD_XSTATS
   = OFFLOAD_XSTATS_UNSPEC
   | OFFLOAD_XSTATS_CPU_HIT -- ^ struct rtnl_link_stats64
   deriving (Show,Eq,Enum)

---------------
-- XDP section
---------------

-- #define XDP_FLAGS_UPDATE_IF_NOEXIST (1U << 0)
-- #define XDP_FLAGS_SKB_MODE  (1U << 1)
-- #define XDP_FLAGS_DRV_MODE  (1U << 2)
-- #define XDP_FLAGS_MASK      (XDP_FLAGS_UPDATE_IF_NOEXIST | \
--                              XDP_FLAGS_SKB_MODE | \
--                              XDP_FLAGS_DRV_MODE)

-- These are stored into IFLA_XDP_ATTACHED on dump.

data XDP_ATTACHED
   = XDP_ATTACHED_NONE
   | XDP_ATTACHED_DRV
   | XDP_ATTACHED_SKB
   deriving (Show,Eq,Enum)

data XDP
   = XDP_UNSPEC
   | XDP_FD
   | XDP_ATTACHED
   | XDP_FLAGS
   deriving (Show,Eq,Enum)
