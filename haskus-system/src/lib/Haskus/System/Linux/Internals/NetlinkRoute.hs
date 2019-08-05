{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- | Netlink routing interface
module Haskus.System.Linux.Internals.NetlinkRoute
   ( MessageType (..)
   , RoutingMessage (..)
   , RoutingType (..)
   , ProtocolType (..)
   , MessageFlag (..)
   , MessageFlags
   , RoutingAttributeType (..)
   , RoutingNextHop (..)
   , NextHopFlag (..)
   , NextHopFlags
   , CacheInfo (..)
   , Metrics (..)
   , Feature (..)
   , Features
   , MFCStats (..)
   , LinkInfo (..)
   , PrefixInfo (..)
   , PrefixType (..)
   , PrefixCacheInfo (..)
   , TrafficControl (..)
   , TrafficFlag (..)
   -- * Distances
   , distanceUniverse
   , distanceSite
   , distanceLink
   , distanceHost
   , distanceNowhere
   -- * Reserved tables
   , tableUnspecified
   , tableCompatibility
   , tableDefault
   , tableMain
   , tableLocal
   )
where

import Haskus.Utils.Types.Generics (Generic)
import Haskus.Utils.Flow
import Haskus.Binary.Storable
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Enum
import Haskus.Binary.BitSet

-- =============================================================
--    From linux/include/uapi/linux/rtnetlink.h
-- =============================================================


data MessageType
   = LinkCreate
   | LinkDelete
   | LinkGet
   | LinkSet
   | AddressCreate
   | AddressDelete
   | AddressGet
   | RouteCreate
   | RouteDelete
   | RouteGet
   | NeighbourCreate
   | NeighbourDelete
   | NeighbourGet
   | RuleCreate
   | RuleDelete
   | RuleGet
   | QDiscCreate
   | QDiscDelete
   | QDiscGet
   | TClassCreate
   | TClassDelete
   | TClassGet
   | FilterCreate
   | FilterDelete
   | FilterGet
   | ActionCreate
   | ActionDelete
   | ActionGet
   | PrefixCreate
   | MulticastGet
   | UnicastGet
   | NeighbourTableCreate
   | NeighbourTableDelete
   | NeighbourTableSet
   | NDUserOption
   | AddressLabelCreate
   | AddressLabelDelete
   | AddressLabelGet
   | DCBGet
   | DCBSet
   | NetconfCreate
   | NetconfDelete
   | NetconfGet
   | MDBCreate
   | MDBDelete
   | MDBGet
   | NSIDCreate
   | NSIDDelete
   | NSIDGet
   | StatsCreate
   | StatsGet
   | CacheReportCreate
   deriving (Show,Eq,Enum)

instance CEnum MessageType where
   fromCEnum e = case e of
      LinkCreate           -> 16
      LinkDelete           -> 17
      LinkGet              -> 18
      LinkSet              -> 19
      AddressCreate        -> 20
      AddressDelete        -> 21
      AddressGet           -> 22
      RouteCreate          -> 24
      RouteDelete          -> 25
      RouteGet             -> 26
      NeighbourCreate      -> 28
      NeighbourDelete      -> 29
      NeighbourGet         -> 30
      RuleCreate           -> 32
      RuleDelete           -> 33
      RuleGet              -> 34
      QDiscCreate          -> 36
      QDiscDelete          -> 37
      QDiscGet             -> 38
      TClassCreate         -> 40
      TClassDelete         -> 41
      TClassGet            -> 42
      FilterCreate         -> 44
      FilterDelete         -> 45
      FilterGet            -> 46
      ActionCreate         -> 48
      ActionDelete         -> 49
      ActionGet            -> 50
      PrefixCreate         -> 52
      MulticastGet         -> 58
      UnicastGet           -> 62
      NeighbourTableCreate -> 64
      NeighbourTableDelete -> 66
      NeighbourTableSet    -> 67
      NDUserOption         -> 68
      AddressLabelCreate   -> 72
      AddressLabelDelete   -> 73
      AddressLabelGet      -> 74
      DCBGet               -> 78
      DCBSet               -> 79
      NetconfCreate        -> 80
      NetconfDelete        -> 81
      NetconfGet           -> 82
      MDBCreate            -> 84
      MDBDelete            -> 85
      MDBGet               -> 86
      NSIDCreate           -> 88
      NSIDDelete           -> 89
      NSIDGet              -> 90
      StatsCreate          -> 92
      StatsGet             -> 94
      CacheReportCreate    -> 96

   toCEnum = error "toCEnum for MessageType is not implemented"

-- | Routing message
--
-- struct rtmsg
data RoutingMessage = RoutingMessage
   { routingMessageFamily            :: !Word8
   , routingMessageDestinationLength :: !Word8
   , routingMessageSourceLength      :: !Word8
   , routingMessageTOS               :: !Word8
   , routingMessageTableId           :: !Word8 -- ^ Routing table ID
   , routingMessageProtocol          :: !Word8 -- ^ Routing protocol
   , routingMessageDistance          :: !Word8
   , routingMessageType              :: !Word8
   , routingMessageFlags             :: !Word32
   }
   deriving (Generic,Storable)


-- | Routing type
data RoutingType
   = RoutingUnspecified
   | RoutingUnicast          -- ^ Gateway or direct route
   | RoutingLocal            -- ^ Accept locally
   | RoutingBroadcast        -- ^ Accept locally as broadcast, send as broadcast
   | RoutingAnycast          -- ^ Accept locally as broadcast, send as unicast
   | RoutingMulticast        -- ^ Multicast route
   | RoutingBlackhole        -- ^ Drop
   | RoutingUnreachable      -- ^ Destination is unreachable
   | RoutingProhibit         -- ^ Administratively prohibited
   | RoutingThrow            -- ^ Not in this table
   | RoutingNAT              -- ^ Translate this address
   | RoutingExternalResolver -- ^ Use external resolver
   | Routing
   deriving (Show,Eq,Enum,CEnum)

-- | Routing protocol
--
-- Values of protocol >= ProtocolStatic are not interpreted by kernel;
-- they are just passed from user and back as is.  It will be used by
-- hypothetical multiple routing daemons.  Note that protocol values
-- should be standardized in order to avoid conflicts.
data ProtocolType
   = ProtocolUnspecified
   | ProtocolRedirect    -- ^ Route installed by ICMP redirects; not used by current IPv4
   | ProtocolKernel      -- ^ Route installed by kernel
   | ProtocolBoot        -- ^ Route installed during boot
   | ProtocolStatic      -- ^ Route installed by administrator
   | ProtocolGated       -- ^ Apparently, GateD
   | ProtocolRA          -- ^ RDISC/ND router advertisements
   | ProtocolMRT         -- ^ Merit MRT
   | ProtocolZebra       -- ^ Zebra
   | ProtocolBird        -- ^ Bird
   | ProtocolDNRouted    -- ^ DECnet routing daemon
   | ProtocolXORP        -- ^ XORP
   | ProtocolNTK         -- ^ Netsukuku
   | ProtocolDHCP        -- ^ DHCP client
   | ProtocolMulticast   -- ^ Multicast daemon
   | ProtocolBabel       -- ^ Babel daemon
   deriving (Show,Eq,Enum)

instance CEnum ProtocolType where
   fromCEnum e = case e of
      ProtocolUnspecified -> 0
      ProtocolRedirect    -> 1
      ProtocolKernel      -> 2
      ProtocolBoot        -> 3
      ProtocolStatic      -> 4
      ProtocolGated       -> 8
      ProtocolRA          -> 9
      ProtocolMRT         -> 10
      ProtocolZebra       -> 11
      ProtocolBird        -> 12
      ProtocolDNRouted    -> 13
      ProtocolXORP        -> 14
      ProtocolNTK         -> 15
      ProtocolDHCP        -> 16
      ProtocolMulticast   -> 17
      ProtocolBabel       -> 42
   toCEnum e = case e of
    0  -> ProtocolUnspecified
    1  -> ProtocolRedirect
    2  -> ProtocolKernel
    3  -> ProtocolBoot
    4  -> ProtocolStatic
    8  -> ProtocolGated
    9  -> ProtocolRA
    10 -> ProtocolMRT
    11 -> ProtocolZebra
    12 -> ProtocolBird
    13 -> ProtocolDNRouted
    14 -> ProtocolXORP
    15 -> ProtocolNTK
    16 -> ProtocolDHCP
    17 -> ProtocolMulticast
    42 -> ProtocolBabel
    _  -> error ("Invalid protocol type value: " ++ show (fromIntegral e :: Integer))


-----------------------
-- Distances ("scope")
-----------------------

-- | Everywhere in the universe
distanceUniverse :: Word8
distanceUniverse = 0

-- | On site
distanceSite :: Word8
distanceSite = 200

-- | Located on directly attached link
distanceLink :: Word8
distanceLink = 253

-- | Our local addresses
distanceHost :: Word8
distanceHost = 254

-- | Reserved for not existing destinations
distanceNowhere :: Word8
distanceNowhere = 255


-- | Flags
data MessageFlag
   = FlagNotify      -- ^ Notify user of route change
   | FlagCloned      -- ^ This route is cloned
   | FlagEqualize    -- ^ Multipath equalizer: NI
   | FlagPrefix      -- ^ Prefix addresses
   | FlagLookupTable -- ^ Set table to FIB lookup result
   | FlagFIBMatch    -- ^ return full FIB lookup match
   deriving (Show,Eq,Enum)

instance CEnum MessageFlag where
   toCEnum e   = toEnum (fromIntegral e - 8)
   fromCEnum e = fromIntegral <| fromEnum e + 8

type MessageFlags = BitSet Word32 MessageFlag

-------------------------------
-- Reserved table identifiers
-------------------------------

-- | Unspecified table
tableUnspecified :: Word8
tableUnspecified = 0

-- | Compatibility table
tableCompatibility :: Word8
tableCompatibility = 252

-- | Default table
tableDefault :: Word8
tableDefault = 253

-- | Main table
tableMain :: Word8
tableMain = 254

-- | Local table
tableLocal :: Word8
tableLocal = 255

data RoutingAttributeType
   = AttrUnspecified
   | AttrDestination
   | AttrSource
   | AttrIIF
   | AttrOIF
   | AttrGateway
   | AttrPriority
   | AttrPreferredSource
   | AttrMetrics
   | AttrMultipath
   | AttrProtoInfo -- no longer used
   | AttrFlow
   | AttrCacheInfo
   | AttrSession -- no longer used
   | AttrMpAlgo -- no longer used
   | AttrTable
   | AttrMark
   | AttrMFCStats
   | AttrVia
   | AttrNewDestination
   | AttrPref
   | AttrEncapType
   | AttrEncap
   | AttrExpires
   | AttrPad
   | AttrUID
   | AttrTtlPropagate
   deriving (Show,Eq,Enum,CEnum)

data RoutingNextHop = RoutingNextHop
   { nextHopLength         :: !Word16
   , nextHopFlags          :: !NextHopFlags
   , nextHopHops           :: !Word8
   , nextHopInterfaceIndex :: !Int
   }
   deriving (Generic,Storable)

data NextHopFlag
   = NextHopDead        -- ^ Nexthop is dead (used by multipath)
   | NextHopPervasive   -- ^ Do recursive gateway lookup
   | NextHopOnLink      -- ^ Gateway is forced on link
   | NextHopOffload     -- ^ Offloaded route
   | NextHopLinkDown    -- ^ Carrier-down on nexthop
   | NextHopUnresolved  -- ^ The entry is unresolved (ipmr)
   deriving (Show,Eq,Enum,CEnum)

type NextHopFlags = BitSet Word8 NextHopFlag


-- skipped: struct rtvia


data CacheInfo = CacheInfo
   { cacheInfoClntRef :: !Word32
   , cacheInfoLastUse :: !Word32
   , cacheInfoExpires :: !Int32
   , cacheInfoError   :: !Word32
   , cacheInfoUsed    :: !Word32
   , cacheInfoId      :: !Word32
   , cacheInfoTs      :: !Word32
   , cacheInfoTsAge   :: !Word32
   }
   deriving (Generic,Storable)

data Metrics
   = MetricsUnspecified
   | MetricsLock
   | MetricsMTU
   | MetricsWindow
   | MetricsRTT
   | MetricsRTTVar
   | MetricsSSThreshold
   | MetricsCwnd
   | MetricsADVMSS
   | MetricsReordering
   | MetricsHopLimit
   | MetricsInitCwnd
   | MetricsFeatures
   | MetricsRTOMin
   | MetricsInitRwnd
   | MetricsQuickAck
   | MetricsCCAlgo
   | MetricsFastopenNoCookie
   deriving (Show,Eq,Enum,CEnum)

data Feature
   = FeatureECN
   | FeatureSACK
   | FeatureTimeStamp
   | FeatureAllFrag
   deriving (Show,Eq,Enum,CEnum)

type Features = BitSet Word8 Feature

-- skipped: rta_session

data MFCStats = MFCStats
   { mfcStatsPackets        :: !Word64
   , mfcStatsBytes          :: !Word64
   , mfcStatsWrongInterface :: !Word64
   }
   deriving (Generic,Storable)


-- | Link level specific information
--
-- struct ifinfomsg
data LinkInfo = LinkInfo
   { linkFamily         :: !Word8
   , linkType           :: !Word16
   , linkIndex          :: !Int -- ^ Link index
   , linkFlags          :: !Word 
   , linkFlagChangeMask :: !Word
   }
   deriving (Generic,Storable)


-- | Prefix information
--
-- struct prefixmsg
data PrefixInfo = PrefixInfo
   { prefixFamily         :: !Word8
   , prefixInterfaceIndex :: !Int
   , prefixType           :: !Word8
   , prefixLength         :: !Word8
   , prefixFlags          :: !Word8
   }
   deriving (Generic,Storable)

data PrefixType
   = PrefixTypeUnspecified
   | PrefixTypeAddress
   | PrefixTypeCacheInfo
   deriving (Show,Eq,Enum,CEnum)

data PrefixCacheInfo = PrefixCacheInfo
   { prefixCacheInfoPreferredTime :: !Word32
   , prefixCacheInfoValidTime     :: !Word32
   }
   deriving (Generic,Storable)

-- | Traffic control message
data TrafficControl = TrafficControl
   { trafficFamily         :: !Word8
   , trafficInterfaceIndex :: !Int
   , trafficHandle         :: !Word32
   , trafficParent         :: !Word32
   , trafficInfo           :: !Word32
   }
   deriving (Generic,Storable)

data TrafficFlag
   = TrafficUnspecified
   | TrafficKind
   | TrafficOptions
   | TrafficStats
   | TrafficXStats
   | TrafficRate
   | TrafficFcnt
   | TrafficStats2
   | TrafficStab
   | TrafficPad
   | TrafficDumpInvisible
   | TrafficChain
   | TrafficHwOffload
   deriving (Show,Eq,Enum,CEnum)

-- skipped: nduseroptmsg, multicast, TC action...
