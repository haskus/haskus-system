{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | DRM/KMS Internals
--
-- Bindings with C structures and IOCTLs
module Haskus.System.Linux.Internals.Graphics
   (
   -- * Mode
     ModeType (..)
   , ModeTypes
   , ModeFlag (..)
   , BasicModeFlags
   , Stereo3D (..)
   , ModeFlags
   , ContentType (..)
   , PowerState(..)
   , ScalingMode(..)
   , AspectRatio(..)
   , DitheringMode(..)
   , DirtyMode(..)
   , LinkStatus (..)
   , ContentProtection (..)
   , RotateReflect (..)
   , StructMode (..)
   , emptyStructMode
   -- * Resources
   , StructCardRes (..)
   -- * Controller
   , StructController (..)
   -- * Plane
   , ModeFieldPresent (..)
   , ModeFieldPresents
   , StructSetPlane (..)
   , StructGetPlane (..)
   , StructGetPlaneRes (..)
   -- * Encoder
   , EncoderType (..)
   , StructGetEncoder (..)
   -- * Connector
   , SubConnectorType (..)
   , ConnectorType (..)
   , StructGetConnector (..)
   -- * Properties
   , PropertyTypeType (..)
   , getPropertyTypeType
   , isImmutable
   , isAtomic
   , StructPropertyEnum (..)
   , StructGetProperty (..)
   , StructSetProperty (..)
   , StructGetObjectProperties (..)
   , StructSetObjectProperty (..)
   , StructGetBlob (..)
   , ObjectType (..)
   -- * Frame
   , FrameFlag (..)
   , FrameFlags
   , StructFrameCommand (..)
   , DirtyAnnotation (..)
   , dirtyMaxClips
   , StructFrameDirty (..)
   , StructModeCommand (..)   -- move
   -- * Cursor
   , CursorFlag (..)
   , CursorFlags
   , StructCursor (..)
   , StructCursor2 (..)
   -- * Gamma look-up table
   , StructControllerLut (..)
   , StructColorCtm (..)
   , StructColorLut (..)
   -- * Frame switching
   , SwitchFrameFlag (..)
   , SwitchFrameFlags
   , StructSwitchFrame (..)
   , StructSwitchFrameTarget (..)
   -- * Generic (dumb) buffer
   , StructCreateDumb (..)
   , StructMapDumb (..)
   , StructDestroyDumb (..)
   -- * Atomic
   , AtomicFlag (..)
   , AtomicFlags
   , StructAtomic (..)
   -- * Blob
   , StructCreateBlob (..)
   , StructDestroyBlob (..)
   , Rect (..)
   -- * Leases
   , StructCreateLease (..)
   , StructRevokeLease (..)
   , StructGetLease (..)
   , StructListLessees (..)
   -- * Generic
   , Clip (..)
   -- * Capabilities
   , Capability (..)
   , StructGetCap (..)
   , ClientCapability (..)
   , StructSetClientCap (..)
   -- * Prime
   , StructPrimeHandle (..)
   , PrimeFlag (..)
   -- * IOCTLs
   , ioctlGetCapabilities
   , ioctlSetClientCapability
   , ioctlGetResources
   , ioctlGetController
   , ioctlSetController
   , ioctlGetGamma
   , ioctlSetGamma
   , ioctlGetEncoder
   , ioctlGetConnector
   , ioctlGetProperty
   , ioctlSetProperty
   , ioctlGetObjectProperties
   , ioctlSetObjectProperty
   , ioctlGetBlob
   , ioctlSwitchFrame
   , ioctlDirtyFrame
   , ioctlCreateHostBuffer
   , ioctlMapHostBuffer
   , ioctlDestroyHostBuffer
   , ioctlGetPlaneResources
   , ioctlGetPlane
   , ioctlSetPlane
   , ioctlAddFrame
   , ioctlRemoveFrame
   , ioctlCursor
   , ioctlAtomic
   , ioctlCreateBlob
   , ioctlDestroyBlob
   -- * Events
   , EventHeader (..)
   , EventType (..)
   , toEventType
   , EventData (..)
   , SequenceEventData (..)
   -- * SubPixel order
   , SubPixel (..)
   )
where

import Haskus.System.Linux.Ioctl
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Internals.Error
import Haskus.System.Linux.Graphics.PixelFormat

import Haskus.Format.Binary.BitSet as BitSet
import Haskus.Format.Binary.Vector as Vector
import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.FixedPoint
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Storable
import Haskus.Format.String
import Haskus.Utils.Flow
import Haskus.Utils.Types.Generics (Generic)

-- =============================================================
--    From linux/include/uapi/drm/drm_mode.h
-- =============================================================

-----------------------------------------------------------------------------
-- Mode
-----------------------------------------------------------------------------

-- | Mode type
data ModeType
   = ModeTypePreferred
   | ModeTypeUserDef
   | ModeTypeDriver
   deriving (Show,Enum)

instance BitOffset ModeType where
   -- DRM used to have more mode types (BUILTIN, CLOCK_C, CRTC_C, DEFAULT) but
   -- they have been deprecated. Hence the gaps in the bitset
   toBitOffset = \case
      ModeTypePreferred -> 3
      ModeTypeUserDef   -> 5
      ModeTypeDriver    -> 6
   fromBitOffset = \case
      3 -> ModeTypePreferred
      5 -> ModeTypeUserDef
      6 -> ModeTypeDriver
      n -> error ("Invalid DRM mode type: " <> show n)

type ModeTypes = BitSet Word32 ModeType


-- | Video mode flags
data ModeFlag
   = ModeFlagPHSync
   | ModeFlagNHSync
   | ModeFlagPVSync
   | ModeFlagNVSync
   | ModeFlagInterlace
   | ModeFlagDoubleScan
   | ModeFlagCSync
   | ModeFlagPCSync
   | ModeFlagNCSync
   | ModeFlagHSkew
   | ModeFlagBroadCast -- ^ Deprecated
   | ModeFlagPixMux    -- ^ Deprecated
   | ModeFlagDoubleClock
   | ModeFlagClockDiv2
   deriving (Show,Enum,BitOffset)


-- | 3D mode
data Stereo3D
   = Stereo3DNone
   | Stereo3DFramePacking
   | Stereo3DFieldAlternative
   | Stereo3DLineAlternative
   | Stereo3DSideBySideFull
   | Stereo3DLDepth
   | Stereo3DLDepthGFXGFXDepth
   | Stereo3DTopAndBottom
   | Stereo3DSideBySideHalf
   deriving (Show,Enum,CEnum)

-- | Aspect ratio
data AspectRatio
   = RatioNone
   | Ratio4_3
   | Ratio16_9
   | Ratio64_27
   | Ratio256_135
   deriving(Show,Eq,Enum,CEnum)

type ModeFlags = BitFields Word32
   '[ BitField 9 ""              Word32
    , BitField 4  "aspect_ratio" (EnumField Word32 AspectRatio)
    , BitField 5  "stereo3d"     (EnumField Word8 Stereo3D)
    , BitField 14 "flags"        BasicModeFlags
    ]

type BasicModeFlags = BitSet Word32 ModeFlag

-- | Content type
data ContentType
   = ContentTypeNoData
   | ContentTypeGraphics
   | ContentTypePhoto
   | ContentTypeCinema
   | ContentTypeGame
   deriving (Show,Eq,Enum)


-- | DPMS flags
data PowerState 
   = PowerOn
   | PowerStandBy
   | PowerSuspend
   | PowerOff
   deriving (Show,Eq,Enum)

-- | Scaling mode
data ScalingMode
   = ScaleNone         -- ^ Unmodified timing (display or software can still scale)
   | ScaleFullScreen   -- ^ Full screen, ignore aspect
   | ScaleCenter       -- ^ Centered, no scaling
   | ScaleAspect       -- ^ Full screen, preserve aspect
   deriving (Show,Eq,Enum)

-- | Dithering mode
data DitheringMode
   = DitheringOff
   | DitheringOn
   | DitheringAuto
   deriving (Show,Eq,Enum)

-- | Dirty mode
data DirtyMode
   = DirtyOff
   | DirtyOn
   | DirtyAnnotate
   deriving (Show,Eq,Enum)

-- | Link status
data LinkStatus
   = LinkStatusGood
   | LinkStatusBad
   deriving (Show,Eq,Enum,CEnum)

-- | Signals that a drm plane is been rotated <degrees> degrees in counter
-- clockwise direction and/or that the contents of a drm plane is reflected
-- along the <axis> axis, in the same way as mirroring.
--
-- See kerneldoc chapter "Plane Composition Properties" for more details.
--
-- This is provided as a convenience, looking up the property id using the
-- name->prop id lookup is the preferred method.
data RotateReflect
   = Rotate0
   | Rotate90
   | Rotate180
   | Rotate270
   | ReflectX
   | ReflectY
   deriving (Show,Eq,Enum,BitOffset)


-- | Content protection
data ContentProtection
   = ProtectionUndesired
   | ProtectionDesired
   | ProtectionEnabled
   deriving (Show,Eq,Enum,CEnum)

-- | drm_mode_modeinfo
data StructMode = StructMode
   { miClock      :: {-# UNPACK #-} !Word32
   , miHDisplay   :: {-# UNPACK #-} !Word16
   , miHSyncStart :: {-# UNPACK #-} !Word16
   , miHSyncEnd   :: {-# UNPACK #-} !Word16
   , miHTotal     :: {-# UNPACK #-} !Word16
   , miHSkew      :: {-# UNPACK #-} !Word16
   , miVDisplay   :: {-# UNPACK #-} !Word16
   , miVSyncStart :: {-# UNPACK #-} !Word16
   , miVSyncEnd   :: {-# UNPACK #-} !Word16
   , miVTotal     :: {-# UNPACK #-} !Word16
   , miVScan      :: {-# UNPACK #-} !Word16
   , miVRefresh   :: {-# UNPACK #-} !Word32
   , miFlags      :: {-# UNPACK #-} !ModeFlags
   , miType       :: {-# UNPACK #-} !ModeTypes
   , miName       :: {-# UNPACK #-} !(CStringBuffer 32)
   } deriving (Generic)

instance Storable StructMode

emptyStructMode :: StructMode
emptyStructMode = StructMode 0 0 0 0 0 0 0 0 0 0 0 0 (BitFields 0) BitSet.empty emptyCStringBuffer

-----------------------------------------------------------------------------
-- Resources
-----------------------------------------------------------------------------

-- | drm_mode_card_res
data StructCardRes = StructCardRes
   { csFbIdPtr    :: {-# UNPACK #-} !Word64
   , csCrtcIdPtr  :: {-# UNPACK #-} !Word64
   , csConnIdPtr  :: {-# UNPACK #-} !Word64
   , csEncIdPtr   :: {-# UNPACK #-} !Word64
   , csCountFbs   :: {-# UNPACK #-} !Word32
   , csCountCrtcs :: {-# UNPACK #-} !Word32
   , csCountConns :: {-# UNPACK #-} !Word32
   , csCountEncs  :: {-# UNPACK #-} !Word32
   , csMinWidth   :: {-# UNPACK #-} !Word32
   , csMaxWidth   :: {-# UNPACK #-} !Word32
   , csMinHeight  :: {-# UNPACK #-} !Word32
   , csMaxHeight  :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- Controller
-----------------------------------------------------------------------------

-- | drm_mode_crtc
data StructController = StructController
   { contSetConnPtr :: {-# UNPACK #-} !Word64
   , contConnCount  :: {-# UNPACK #-} !Word32
   , contID         :: {-# UNPACK #-} !Word32
   , contFbID       :: {-# UNPACK #-} !Word32
   , contFbX        :: {-# UNPACK #-} !Word32
   , contFbY        :: {-# UNPACK #-} !Word32
   , contGammaSize  :: {-# UNPACK #-} !Word32
   , contModeValid  :: {-# UNPACK #-} !Word32
   , contModeInfo   :: {-# UNPACK #-} !StructMode
   } deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- Plane
-----------------------------------------------------------------------------

data ModeFieldPresent
   = PresentTopField
   | PresentBottomField
   deriving (Show,Enum,BitOffset)

type ModeFieldPresents = BitSet Word32 ModeFieldPresent

-- | drm_mode_set_plane
--
-- Planes blend with or override other bits on the CRTC
data StructSetPlane = StructSetPlane
   { spPlaneId :: {-# UNPACK #-} !Word32
   , spCrtcId  :: {-# UNPACK #-} !Word32
   , spFrameId :: {-# UNPACK #-} !Word32 -- ^ Frame contains surface format type
   , spFlags   :: {-# UNPACK #-} !ModeFieldPresents
   , spCrtcX   :: {-# UNPACK #-} !Int32 -- ^ Signed dest location allows it to be partially off screen
   , spCrtcY   :: {-# UNPACK #-} !Int32
   , spCrtcW   :: {-# UNPACK #-} !Word32
   , spCrtcH   :: {-# UNPACK #-} !Word32

   , spSrcX    :: {-# UNPACK #-} !(FixedPoint Word32 16 16)
   , spSrcY    :: {-# UNPACK #-} !(FixedPoint Word32 16 16)
   , spSrcH    :: {-# UNPACK #-} !(FixedPoint Word32 16 16)
   , spSrcW    :: {-# UNPACK #-} !(FixedPoint Word32 16 16)
   } deriving (Generic,Storable)

-- | drm_mode_get_plane
data StructGetPlane = StructGetPlane
   { gpPlaneId       :: {-# UNPACK #-} !Word32
   , gpCrtcId        :: {-# UNPACK #-} !Word32
   , gpFbId          :: {-# UNPACK #-} !Word32
   , gpPossibleCrtcs :: {-# UNPACK #-} !(BitSet Word32 Int)
   , gpGammaSize     :: {-# UNPACK #-} !Word32
   , gpCountFmtTypes :: {-# UNPACK #-} !Word32
   , gpFormatTypePtr :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

-- | drm_mode_get_plane_res
data StructGetPlaneRes = StructGetPlaneRes
   { gprsPlaneIdPtr  :: {-# UNPACK #-} !Word64
   , gprsCountPlanes :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)


-----------------------------------------------------------------------------
-- Encoder
-----------------------------------------------------------------------------

-- | Type of the encoder
data EncoderType
   = EncoderTypeNone
   | EncoderTypeDAC     -- ^ for VGA and analog on DVI-I/DVI-A
   | EncoderTypeTMDS    -- ^ for DVI, HDMI and (embedded) DisplayPort
   | EncoderTypeLVDS    -- ^ for display panels
   | EncoderTypeTVDAC   -- ^ for TV output (Composite, S-Video, Component, SCART)
   | EncoderTypeVirtual -- ^ for virtual machine display
   | EncoderTypeDSI
   | EncoderTypeDPMST
   | EncoderTypeDPI
   deriving (Eq,Ord,Show,Enum,CEnum)

-- | drm_mode_get_encoder
data StructGetEncoder = StructGetEncoder
   { geEncoderId      :: {-# UNPACK #-} !Word32
   , geEncoderType    :: {-# UNPACK #-} !(EnumField Word32 EncoderType)
   , geCrtcId         :: {-# UNPACK #-} !Word32
   , gePossibleCrtcs  :: {-# UNPACK #-} !(BitSet Word32 Int) -- ^ Valid controller indexes
   , gePossibleClones :: {-# UNPACK #-} !(BitSet Word32 Int) -- ^ Valid clone encoder indexes
   } deriving (Generic,Storable)

-- | This is for connectors with multiple signal types
data SubConnectorType
   = SubConnectorTypeUnknown
   | SubConnectorTypeDVID
   | SubConnectorTypeDVIA
   | SubConnectorTypeComposite
   | SubConnectorTypeSVIDEO
   | SubConnectorTypeComponent
   | SubConnectorTypeSCART
   deriving (Show,Eq)

-- Try to match ConnectorType as closely as possible...
instance Enum SubConnectorType where
   fromEnum x = case x of
      SubConnectorTypeUnknown    -> 0
      SubConnectorTypeDVID       -> 3
      SubConnectorTypeDVIA       -> 4
      SubConnectorTypeComposite  -> 5
      SubConnectorTypeSVIDEO     -> 6
      SubConnectorTypeComponent  -> 8
      SubConnectorTypeSCART      -> 9
   toEnum x = case x of
      0 -> SubConnectorTypeUnknown
      3 -> SubConnectorTypeDVID
      4 -> SubConnectorTypeDVIA
      5 -> SubConnectorTypeComposite
      6 -> SubConnectorTypeSVIDEO
      8 -> SubConnectorTypeComponent
      9 -> SubConnectorTypeSCART
      _ -> error "Unknown sub-connector type"

-- | Connector type
data ConnectorType
   = ConnectorTypeUnknown
   | ConnectorTypeVGA
   | ConnectorTypeDVII
   | ConnectorTypeDVID
   | ConnectorTypeDVIA
   | ConnectorTypeComposite
   | ConnectorTypeSVIDEO
   | ConnectorTypeLVDS
   | ConnectorTypeComponent
   | ConnectorType9PinDIN
   | ConnectorTypeDisplayPort
   | ConnectorTypeHDMIA
   | ConnectorTypeHDMIB
   | ConnectorTypeTV
   | ConnectorTypeeDP
   | ConnectorTypeVirtual
   | ConnectorTypeDSI
   | ConnectorTypeDPI
   | ConnectorTypeWriteback   -- ^ CRTC output is written back into memory (connector always appears disconnected)
   deriving (Eq, Ord, Enum, CEnum)

instance Show ConnectorType where
   show x = case x of
      ConnectorTypeUnknown       -> "Unknown"
      ConnectorTypeVGA           -> "VGA"
      ConnectorTypeDVII          -> "DVI-I"
      ConnectorTypeDVID          -> "DVI-D"
      ConnectorTypeDVIA          -> "DVI-A"
      ConnectorTypeComposite     -> "Composite"
      ConnectorTypeSVIDEO        -> "SVIDEO"
      ConnectorTypeLVDS          -> "LVDS"
      ConnectorTypeComponent     -> "Component"
      ConnectorType9PinDIN       -> "9PinDIN"
      ConnectorTypeDisplayPort   -> "DisplayPort"
      ConnectorTypeHDMIA         -> "HDMI-A"
      ConnectorTypeHDMIB         -> "HDMI-B"
      ConnectorTypeTV            -> "TV"
      ConnectorTypeeDP           -> "eDP"
      ConnectorTypeVirtual       -> "Virtual"
      ConnectorTypeDSI           -> "DSI"
      ConnectorTypeDPI           -> "DPI"
      ConnectorTypeWriteback     -> "Writeback"

-- | drm_mode_get_connector
data StructGetConnector = StructGetConnector
   { connEncodersPtr       :: {-# UNPACK #-} !Word64
   , connModesPtr          :: {-# UNPACK #-} !Word64
   , connPropsPtr          :: {-# UNPACK #-} !Word64
   , connPropValuesPtr     :: {-# UNPACK #-} !Word64

   , connModesCount        :: {-# UNPACK #-} !Word32
   , connPropsCount        :: {-# UNPACK #-} !Word32
   , connEncodersCount     :: {-# UNPACK #-} !Word32

   , connEncoderID_        :: {-# UNPACK #-} !Word32   -- ^ current encoder
   , connConnectorID_      :: {-# UNPACK #-} !Word32   -- ^ ID
   , connConnectorType_    :: {-# UNPACK #-} !(EnumField Word32 ConnectorType)
   , connConnectorTypeID_  :: {-# UNPACK #-} !Word32

   , connConnection_       :: {-# UNPACK #-} !Word32
   , connWidth_            :: {-# UNPACK #-} !Word32   -- ^ HxW in millimeters
   , connHeight_           :: {-# UNPACK #-} !Word32
   , connSubPixel_         :: {-# UNPACK #-} !(EnumField Word32 SubPixel)
   } deriving (Generic,Storable)


-----------------------------------------------------------------------------
-- Properties
-----------------------------------------------------------------------------

-- | Type of the property
data PropertyTypeType
   = PropTypeRange
   | PropTypeEnum       -- ^ Enumerated type with text strings
   | PropTypeBlob
   | PropTypeBitmask    -- ^ Bitmask of enumerated types
   | PropTypeObject
   | PropTypeSignedRange
   deriving (Eq,Ord,Show)

getPropertyTypeType :: StructGetProperty -> PropertyTypeType
getPropertyTypeType x =
   -- type is interleaved with Pending and Immutable flags
   case gpsFlags x .&. 0xFA of
      2   -> PropTypeRange
      8   -> PropTypeEnum
      16  -> PropTypeBlob
      32  -> PropTypeBitmask
      64  -> PropTypeObject
      128 -> PropTypeSignedRange
      _   -> error "Unknown property type"

isImmutable :: StructGetProperty -> Bool
isImmutable x = testBit (gpsFlags x) 2

isAtomic :: StructGetProperty -> Bool
isAtomic x = testBit (gpsFlags x) 31

-- | drm_mode_property_enum
data StructPropertyEnum = StructPropertyEnum
   { peValue       :: {-# UNPACK #-} !Word64
   , peName        :: {-# UNPACK #-} !(CStringBuffer 32)
   } deriving (Generic,Storable)

data ObjectType
   = ObjectController
   | ObjectConnector
   | ObjectEncoder
   | ObjectMode
   | ObjectProperty
   | ObjectFrame
   | ObjectBlob
   | ObjectPlane
   deriving (Show,Eq,Ord,Enum)

instance CEnum ObjectType where
   toCEnum x = case x of
      0xcccccccc -> ObjectController
      0xc0c0c0c0 -> ObjectConnector
      0xe0e0e0e0 -> ObjectEncoder
      0xdededede -> ObjectMode
      0xb0b0b0b0 -> ObjectProperty
      0xfbfbfbfb -> ObjectFrame
      0xbbbbbbbb -> ObjectBlob
      0xeeeeeeee -> ObjectPlane
      _          -> error "Invalid object type"

   fromCEnum x = case x of
      ObjectController   -> 0xcccccccc
      ObjectConnector    -> 0xc0c0c0c0
      ObjectEncoder      -> 0xe0e0e0e0
      ObjectMode         -> 0xdededede
      ObjectProperty     -> 0xb0b0b0b0
      ObjectFrame        -> 0xfbfbfbfb
      ObjectBlob         -> 0xbbbbbbbb
      ObjectPlane        -> 0xeeeeeeee

-- | drm_mode_get_property
data StructGetProperty = StructGetProperty
   { gpsValuesPtr      :: {-# UNPACK #-} !Word64 -- ^ Values or blob lengths
   , gpsEnumBlobPtr    :: {-# UNPACK #-} !Word64 -- ^ Enum or blob id ptrs
   , gpsPropId         :: {-# UNPACK #-} !Word32
   , gpsFlags          :: {-# UNPACK #-} !Word32
   , gpsName           :: {-# UNPACK #-} !(CStringBuffer 32)
   , gpsCountValues    :: {-# UNPACK #-} !Word32
   , gpsCountEnum      :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)

-- | drm_mode_set_property
data StructSetProperty = StructSetProperty
   { spsValue        :: {-# UNPACK #-} !Word64
   , spsPropId       :: {-# UNPACK #-} !Word32
   , spsConnId       :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)

-- | drm_mode_obj_get_properties
data StructGetObjectProperties = StructGetObjectProperties
   { gopPropsPtr        :: {-# UNPACK #-} !Word64
   , gopValuesPtr       :: {-# UNPACK #-} !Word64
   , gopCountProps      :: {-# UNPACK #-} !Word32
   , gopObjId           :: {-# UNPACK #-} !Word32
   , gopObjType         :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)

-- | drm_mode_obj_set_property
data StructSetObjectProperty = StructSetObjectProperty
   { sopValue           :: {-# UNPACK #-} !Word64
   , sopPropId          :: {-# UNPACK #-} !Word32
   , sopObjId           :: {-# UNPACK #-} !Word32
   , sopObjType         :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)

-- | drm_mode_get_blob
data StructGetBlob = StructGetBlob
   { gbBlobId     :: {-# UNPACK #-} !Word32
   , gbLength     :: {-# UNPACK #-} !Word32
   , gbData       :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- Frame
-----------------------------------------------------------------------------

-- we don't use drm_mode_fb_cmd as we have drm_mode_fb_cmd2

-- | Frame flags
data FrameFlag
   = FrameInterlaced    -- ^ Interlaced frame
   | FrameUseModifiers  -- ^ Enable modifiers
   deriving (Show,Eq,Enum,BitOffset)

type FrameFlags = BitSet Word32 FrameFlag

-- | Data matching the C structure drm_mode_fb_cmd2
data StructFrameCommand = StructFrameCommand
   { fc2FbId          :: {-# UNPACK #-} !Word32
   , fc2Width         :: {-# UNPACK #-} !Word32
   , fc2Height        :: {-# UNPACK #-} !Word32
   , fc2PixelFormat   :: {-# UNPACK #-} !PixelFormat
   , fc2Flags         :: {-# UNPACK #-} !FrameFlags
   , fc2Handles       :: {-# UNPACK #-} !(Vector 4 Word32)
   , fc2Pitches       :: {-# UNPACK #-} !(Vector 4 Word32)  -- ^ Pitch for each plane
   , fc2Offsets       :: {-# UNPACK #-} !(Vector 4 Word32)  -- ^ Offset of each plane
   , fc2Modifiers     :: {-# UNPACK #-} !(Vector 4 Word64)  -- ^ tiling, compressed
   } deriving (Generic,Storable)

-- | Mark a region of a framebuffer as dirty.
-- 
-- Some hardware does not automatically update display contents as a hardware or
-- software draw to a framebuffer. This ioctl allows userspace to tell the
-- kernel and the hardware what regions of the framebuffer have changed.
-- 
-- The kernel or hardware is free to update more then just the region specified
-- by the clip rects. The kernel or hardware may also delay and/or coalesce
-- several calls to dirty into a single update.
-- 
-- Userspace may annotate the updates, the annotates are a promise made by the
-- caller that the change is either a copy of pixels or a fill of a single color
-- in the region specified.
-- 
-- If the DirtyCopy mode is used then the clip rects are paired as (src,dst).
-- The width and height of each one of the pairs must match.
-- 
-- If the DirtyFill mode is used the caller promises that the region specified
-- of the clip rects is filled completely with a single color as given in the
-- color argument.
data DirtyAnnotation
   = Dirty     [Clip]
   | DirtyCopy [(Clip,Clip)]
   | DirtyFill Word32 [Clip]
   deriving (Show,Eq)

dirtyMaxClips :: Word32
dirtyMaxClips = 256

-- | drm_mode_fb_dirty_cmd
data StructFrameDirty = StructFrameDirty
   { fdFbId          :: {-# UNPACK #-} !Word32
   , fdFlags         :: {-# UNPACK #-} !Word32
   , fdColor         :: {-# UNPACK #-} !Word32
   , fdNumClips      :: {-# UNPACK #-} !Word32
   , fdClipsPtr      :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

-- | drm_mode_mode_cmd
data StructModeCommand = StructModeCommand
   { mcConnId     :: {-# UNPACK #-} !Word32
   , mcMode       :: {-# UNPACK #-} !StructMode
   } deriving (Generic,Storable)


-----------------------------------------------------------------------------
-- Cursor
-----------------------------------------------------------------------------

-- | Depending on the value in flags different members are used.
-- 
-- CursorFlagBO uses
--    crtcId
--    width
--    height
--    handle - if 0 turns the cursor off
-- 
-- CursorFlagMove uses
--    crtcId
--    x
--    y
data CursorFlag
   = CursorFlagBO
   | CursorFlagMove
   | CursorFlagFlags
   deriving (Eq,Enum,Show,BitOffset)

type CursorFlags = BitSet Word32 CursorFlag

-- | drm_mode_cursor
data StructCursor = StructCursor
   { curFlags     :: {-# UNPACK #-} !CursorFlags
   , curCrtcId    :: {-# UNPACK #-} !Word32
   , curX         :: {-# UNPACK #-} !Int32
   , curY         :: {-# UNPACK #-} !Int32
   , curWidth     :: {-# UNPACK #-} !Word32
   , curHeight    :: {-# UNPACK #-} !Word32
   , curHandle    :: {-# UNPACK #-} !Word32 -- ^ Driver specific handle
   } deriving (Generic,Storable)

-- | drm_mode_cursor2
data StructCursor2 = StructCursor2
   { cur2Flags     :: {-# UNPACK #-} !CursorFlags
   , cur2CrtcId    :: {-# UNPACK #-} !Word32
   , cur2X         :: {-# UNPACK #-} !Int32
   , cur2Y         :: {-# UNPACK #-} !Int32
   , cur2Width     :: {-# UNPACK #-} !Word32
   , cur2Height    :: {-# UNPACK #-} !Word32
   , cur2Handle    :: {-# UNPACK #-} !Word32 -- ^ Driver specific handle
   , cur2HotX      :: {-# UNPACK #-} !Int32
   , cur2HotY      :: {-# UNPACK #-} !Int32
   } deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- Gamma look-up table
-----------------------------------------------------------------------------

-- | drm_mode_crtc_lut
data StructControllerLut = StructControllerLut
   { clsCrtcId       :: {-# UNPACK #-} !Word32
   , clsGammaSize    :: {-# UNPACK #-} !Word32
   , clsRed          :: {-# UNPACK #-} !Word64 -- ^ Pointers to arrays
   , clsGreen        :: {-# UNPACK #-} !Word64
   , clsBlue         :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

newtype StructColorCtm = StructColorCtm
   { colorCtmMatrix :: Vector 9 Word64 -- ^ Conversion matrix in S31.32 sign-magnitude (not two's complement!) format.
   }

-- | Values are mapped linearly to 0.0 - 1.0 range, with 0x0 == 0.0 and 0xffff == 1.0.
data StructColorLut = StructColorLut
   { colorLutRed      :: !Word16
   , colorLutGreen    :: !Word16
   , colorLutBlue     :: !Word16
   , colorLutReserved :: !Word16
   }

-----------------------------------------------------------------------------
-- Frame switching
-----------------------------------------------------------------------------

-- | Frame switching flags
data SwitchFrameFlag
   = SwitchFrameGenerateEvent  -- ^ Generate an event when the frame is switched
   | SwitchFrameAsync          -- ^ Switch the frame without waiting for VBlank interval (may not be supported)
   | SwitchFrameTargetAbsolute
   | SwitchFrameTargetRelative
   deriving (Show,Eq,Enum,BitOffset)

type SwitchFrameFlags = BitSet Word32 SwitchFrameFlag

-- 
-- Request a frame switch on the specified controller.
-- 
-- This ioctl will ask KMS to schedule a frame switch for the specified
-- controller. Once any pending rendering targeting the specified frame (as of
-- ioctl time) has completed, the controller will be reprogrammed to display
-- that frame after the next vertical refresh. The ioctl returns immediately,
-- but subsequent rendering to the current frame will block in the execbuffer
-- ioctl until the frame switch happens. If a frame switch is already pending as
-- the ioctl is called, EBUSY will be returned.
-- 
-- `SwitchFrameGenerateEvent` flag requests that drm sends back an event when
-- the frame switch done. The user_data field passed in with this ioctl will be
-- returned as the user_data field in the event.
-- 
-- `SwitchFrameAsync` flag requests that the switch happen 'as soon as
-- possible', meaning that it not delay waiting for vblank.
-- This may cause tearing on the screen.
-- 

-- | drm_mode_crtc_page_flip
data StructSwitchFrame = StructSwitchFrame
   { pfCrtcId        :: {-# UNPACK #-} !Word32
   , pfFrameId       :: {-# UNPACK #-} !Word32
   , pfFlags         :: {-# UNPACK #-} !SwitchFrameFlags
   , pfReserved      :: {-# UNPACK #-} !Word32
   , pfUserData      :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

--
-- Request a frame switch on the specified controller.
--
-- Same as StructSwitchFrame, but supports new flags and re-purposes the
-- reserved field:
--
-- The sequence field must be zero unless either of the
-- `SwitchFrameTargetAbsolute` or `SwitchFrameTargetRelative` flags is
-- specified.
--    * When the ABSOLUTE flag is specified, the sequence field denotes the
--    absolute vblank sequence when the frame switch should take effect.
--    * When the RELATIVE flag is specified, the sequence field denotes the
--    relative (to the current one when the ioctl is called) vblank sequence
--    when the frame switch should take effect.
--
-- NOTE: DRM_IOCTL_WAIT_VBLANK must still be used to make sure the
-- vblank sequence before the target one has passed before calling this ioctl.
-- The purpose of the SwitchFrameTargetAbsolute/Relative flags is merely to
-- clarify the target for when code dealing with a frame switch runs during a
-- vertical blank period.

-- drm_mode_crtc_page_flip_target
data StructSwitchFrameTarget = StructSwitchFrameTarget
   { pftCrtcId   :: {-# UNPACK #-} !Word32
   , pftFrameId  :: {-# UNPACK #-} !Word32
   , pftFlags    :: {-# UNPACK #-} !SwitchFrameFlags
   , pftSequence :: {-# UNPACK #-} !Word32
   , pftUserData :: {-# UNPACK #-} !Word64
   } deriving (Show,Generic,Storable)

-----------------------------------------------------------------------------
-- Generic buffer
-----------------------------------------------------------------------------

-- | drm_mode_create_dumb
data StructCreateDumb = StructCreateDumb
   { cdHeight :: {-# UNPACK #-} !Word32
   , cdWidth  :: {-# UNPACK #-} !Word32
   , cdBPP    :: {-# UNPACK #-} !Word32 -- ^ Bits per pixel
   , cdFlags  :: {-# UNPACK #-} !Word32
   , cdHandle :: {-# UNPACK #-} !Word32 -- ^ Handle, pitch, size will be returned
   , cdPitch  :: {-# UNPACK #-} !Word32
   , cdSize   :: {-# UNPACK #-} !Word64
   } deriving (Show,Generic,Storable)


-- | drm_mode_map_dumb
data StructMapDumb = StructMapDumb
   { mdHandle :: {-# UNPACK #-} !Word32
   , mdPad    :: {-# UNPACK #-} !Word32  -- Padding field: not useful
   , mdOffset :: {-# UNPACK #-} !Word64  -- ^ Fake offset to use for subsequent mmap call
   } deriving (Show,Generic,Storable)

-- | drm_mode_destroy_dumb
newtype StructDestroyDumb = StructDestroyDumb
   { dbHandle :: Word32 -- ^ Dumb buffer handle
   } deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- Atomic
-----------------------------------------------------------------------------

-- | Flags for the atomic state change
--
-- `AtomicFlagAllowModeset` is useful for devices such as tablets whose screen is
-- often shutdown: we can use a degraded mode (scaled, etc.) for a while to save
-- power and only perform the full modeset when the screen is reactivated.
data AtomicFlag
   = AtomicFlagSwitchFrameGenerateEvent -- ^ Generates an event after a frame switch
   | AtomicFlagSwitchFrameAsync         -- ^ Asynchronous frame switch, i.e. don't wait for v-blank (may not be supported)
   | AtomicFlagTestOnly                 -- ^ Only test the config, don't commit it
   | AtomicFlagNonBlock                 -- ^ Schedule an asynchronous commit (may not be supported)
   | AtomicFlagAllowModeset             -- ^ Allow full mode-setting
   deriving (Show,Eq,Enum)

instance BitOffset AtomicFlag where
   toBitOffset x = case x of
      AtomicFlagSwitchFrameGenerateEvent -> 0
      AtomicFlagSwitchFrameAsync         -> 1
      AtomicFlagTestOnly                 -> 8
      AtomicFlagNonBlock                 -> 9
      AtomicFlagAllowModeset             -> 10
   fromBitOffset x = case x of
      0  -> AtomicFlagSwitchFrameGenerateEvent
      1  -> AtomicFlagSwitchFrameAsync
      8  -> AtomicFlagTestOnly
      9  -> AtomicFlagNonBlock
      10 -> AtomicFlagAllowModeset
      _  -> error "Unknown atomic flag"

-- | Set of atomic flags
type AtomicFlags = BitSet Word32 AtomicFlag

-- | drm_mode_atomic
data StructAtomic = StructAtomic
   { atomFlags         :: {-# UNPACK #-} !AtomicFlags
   , atomCountObjects  :: {-# UNPACK #-} !Word32
   , atomObjectsPtr    :: {-# UNPACK #-} !Word64
   , atomCountPropsPtr :: {-# UNPACK #-} !Word64
   , atomPropsPtr      :: {-# UNPACK #-} !Word64
   , atomPropValuesPtr :: {-# UNPACK #-} !Word64
   , atomReserved      :: {-# UNPACK #-} !Word64
   , atomUserData      :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- Blob
-----------------------------------------------------------------------------

-- | Create a new 'blob' data property, copying length bytes from data pointer,
-- and returning new blob ID.
data StructCreateBlob = StructCreateBlob
   { cbData   :: {-# UNPACK #-} !Word64 -- ^ Pointer to data to copy
   , cbLength :: {-# UNPACK #-} !Word32 -- ^ Length of data to copy
   , cbBlobID :: {-# UNPACK #-} !Word32 -- ^ Return: new property ID
   } deriving (Generic,Storable)

-- | Destroy a user-created blob property.
newtype StructDestroyBlob = StructDestroyBlob
   { dbBlobId :: Word32 -- ^ blob identifier
   } deriving (Generic,Storable)

-- | Lease mode resources, creating another drm_master.
data StructCreateLease = StructCreateLease
   { clObjectIds   :: !Word64 -- ^ Pointer to array of object ids (Word32s)
   , clObjectCount :: !Word32 -- ^ Number of object ids
   , clFlags       :: !Word32 -- ^ Flags for new FD (O_CLOEXEC, etc.)
   , clLesseeId    :: !Word32 -- ^ Returned unique identifier for lessee
   , clLesseeFd    :: !Word32 -- ^ Returned file descriptor to new drm_amster file
   }

-- | List lesses from a drm_master
data StructListLessees = StructListLessees
   { llCountLessees :: !Word32 -- ^ Input: length of the array. Output: total number of objects
   , llReserved     :: !Word32
   , llObjectsPtr   :: !Word64 -- ^ Pointer to object ids (Word32)
   }

-- | Get leased objects
data StructGetLease = StructGetLease
   { glCountObjects :: !Word32 -- ^ Input: length of the array. Output: total number of leesses
   , glReserved     :: !Word32
   , glLesseesPtr   :: !Word64 -- ^ Pointer to lessee ids (Word64)
   }

-- | Revoke lease
newtype StructRevokeLease = StructRevokeLease
   { rlLesseeId :: Word32 -- ^ Unique ID of lessee
   }

-- | Two dimensional rectangle (used by FB_DAMAGE_CLIPS property of atomic blobs)
data Rect = Rect
   { rectX1 :: {-# UNPACK #-} !Int32 -- ^ Horizontal starting coordinate (inclusive)
   , rectY1 :: {-# UNPACK #-} !Int32 -- ^ Vertical starting coordinate (inclusive)
   , rectX2 :: {-# UNPACK #-} !Int32 -- ^ Horizontal ending coordinate (exclusive)
   , rectY2 :: {-# UNPACK #-} !Int32 -- ^ Vertical ending coordinate (exclusive)
   } deriving (Show,Eq,Generic,Storable)

-- =============================================================
--    From linux/include/uapi/drm/drm.h
-- =============================================================

-----------------------------------------------------------------------------
-- Generic
-----------------------------------------------------------------------------

data Clip = Clip
   { clipX1 :: {-# UNPACK #-} !Word16
   , clipY1 :: {-# UNPACK #-} !Word16
   , clipX2 :: {-# UNPACK #-} !Word16
   , clipY2 :: {-# UNPACK #-} !Word16
   } deriving (Show,Eq,Generic,Storable)

-----------------------------------------------------------------------------
-- Capabilities
-----------------------------------------------------------------------------


-- | Capability
data Capability
   = CapHostBuffer         -- ^ Support generic buffers (i.e. not vendor specific)
   | CapVBlankHighController
   | CapGenericPreferredDepth
   | CapGenericPreferShadow
   | CapPrime
   | CapTimestampMonotonic
   | CapAsyncFrameSwitch      -- ^ Support asynchronous frame switching
   | CapCursorWidth           -- ^ Return a valid width plane size to use (may be the maximum, depending on the driver)
   | CapCursorHeight          -- ^ Ditto cursor plane height
   | CapAddFrameModifiers
   | CapSwitchFrameTarget
   | CapControllerInVBlankEvent -- ^ Indicate that the vblank event contains a crtc_id field (always true since Linux 5.1)
   | CapSyncObject
   | CapSyncObjectTimeline
   deriving (Show,Eq,Enum)

-- Add 1 to the enum number to get the valid value
instance CEnum Capability where
   fromCEnum = \case
      CapHostBuffer              -> 0x1
      CapVBlankHighController    -> 0x2
      CapGenericPreferredDepth   -> 0x3
      CapGenericPreferShadow     -> 0x4
      CapPrime                   -> 0x5
      CapTimestampMonotonic      -> 0x6
      CapAsyncFrameSwitch        -> 0x7
      CapCursorWidth             -> 0x8
      CapCursorHeight            -> 0x9
      CapAddFrameModifiers       -> 0x10 -- there is no reason for this gap, except if they forgot that it was hexadecimal...
      CapSwitchFrameTarget       -> 0x11
      CapControllerInVBlankEvent -> 0x12
      CapSyncObject              -> 0x13
      CapSyncObjectTimeline      -> 0x14
   toCEnum = \case
      0x1  -> CapHostBuffer
      0x2  -> CapVBlankHighController
      0x3  -> CapGenericPreferredDepth
      0x4  -> CapGenericPreferShadow
      0x5  -> CapPrime
      0x6  -> CapTimestampMonotonic
      0x7  -> CapAsyncFrameSwitch
      0x8  -> CapCursorWidth
      0x9  -> CapCursorHeight
      0x10 -> CapAddFrameModifiers
      0x11 -> CapSwitchFrameTarget
      0x12 -> CapControllerInVBlankEvent
      0x13 -> CapSyncObject
      0x14 -> CapSyncObjectTimeline
      x    -> error ("Unknown capability flag: " ++ show (fromIntegral x :: Word))

-- | drm_get_cap
--
-- The CURSOR_WIDTH and CURSOR_HEIGHT capabilities return a valid widthxheight
-- combination for the hardware cursor. The intention is that a hardware
-- agnostic userspace can query a cursor plane size to use.
-- 
-- Note that the cross-driver contract is to merely return a valid size; drivers
-- are free to attach another meaning on top, eg. i915 returns the maximum plane
-- size.
-- 
data StructGetCap = StructGetCap
   { gcCapability :: {-# UNPACK #-} !(EnumField Word64 Capability)
   , gcValue      :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

-- | Client capabilities
data ClientCapability
   = ClientCapStereo3D            -- ^ expose the stereo 3D capabilities of the monitor by advertising the supported 3D layouts in the flags of struct drm_mode_modeinfo (cf Stereo3D)
   | ClientCapUniversalPlanes     -- ^ expose all planes (overlay, primary, and cursor) to userspace.
   | ClientCapAtomic              -- ^ expose atomic properties to userspace
   | ClientCapAspectRatio         -- ^ provide apect ratio information in modes
   | ClientCapWritebackConnectors -- ^ expose special connectors to be used for writing back to memory the scene setup in the commit. Depends on client also supporting `ClientCapAtomic`
   deriving (Show,Eq,Enum)

-- Add 1 to the enum number to get the valid value
instance CEnum ClientCapability where
   fromCEnum = (+1) . fromIntegral . fromEnum
   toCEnum   = toEnum . (\x -> x-1) . fromIntegral

data StructSetClientCap = StructSetClientCap
   { sccCapability :: {-# UNPACK #-} !(EnumField Word64 ClientCapability)
   , sccValue      :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

data PrimeFlag
   = PrimeFlagReadWrite
   | PrimeFlagCloseOnExec
   deriving (Show,Eq,BitOffset)

instance Enum PrimeFlag where
   fromEnum PrimeFlagReadWrite   = fromEnum HandleReadWrite
   fromEnum PrimeFlagCloseOnExec = fromEnum HandleCloseOnExec
   toEnum x = case toEnum x of
      HandleReadWrite   -> PrimeFlagReadWrite
      HandleCloseOnExec -> PrimeFlagCloseOnExec
      _                 -> error ("Unknown prime flag: " ++ show x)

-- | struct drm_prime_handle
data StructPrimeHandle = StructPrimeHandle
   { sphHandle :: {-# UNPACK #-} !Word32
   , sphFlags  :: {-# UNPACK #-} !(BitSet Word32 PrimeFlag) -- ^ FD flags: only applciable for handle->fd
   , sphFD     :: {-# UNPACK #-} !Int32                     -- ^ Returned DMAbuf file descriptor
   }
   deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- IOCTLs
-----------------------------------------------------------------------------

drmIoctl :: (MonadInIO m, Storable a) => Word8 -> a -> Handle -> Excepts '[ErrorCode] m a
drmIoctl = ioctlWriteRead 0x64


ioctlGetCapabilities :: MonadInIO m => StructGetCap -> Handle -> Excepts '[ErrorCode] m StructGetCap
ioctlGetCapabilities = drmIoctl 0x0C

ioctlSetClientCapability :: MonadInIO m => StructSetClientCap -> Handle -> Excepts '[ErrorCode] m StructSetClientCap
ioctlSetClientCapability = drmIoctl 0x0D
 
 
ioctlGetResources :: MonadInIO m => StructCardRes -> Handle -> Excepts '[ErrorCode] m StructCardRes
ioctlGetResources = drmIoctl 0xA0

ioctlGetController :: MonadInIO m => StructController -> Handle -> Excepts '[ErrorCode] m StructController
ioctlGetController = drmIoctl 0xA1

ioctlSetController :: MonadInIO m => StructController -> Handle -> Excepts '[ErrorCode] m StructController
ioctlSetController = drmIoctl 0xA2

ioctlGetGamma :: MonadInIO m => StructControllerLut -> Handle -> Excepts '[ErrorCode] m StructControllerLut
ioctlGetGamma = drmIoctl 0xA4

ioctlSetGamma :: MonadInIO m => StructControllerLut -> Handle -> Excepts '[ErrorCode] m StructControllerLut
ioctlSetGamma = drmIoctl 0xA5

ioctlGetEncoder :: MonadInIO m => StructGetEncoder -> Handle -> Excepts '[ErrorCode] m StructGetEncoder
ioctlGetEncoder = drmIoctl 0xA6

ioctlGetConnector :: MonadInIO m => StructGetConnector -> Handle -> Excepts '[ErrorCode] m StructGetConnector
ioctlGetConnector = drmIoctl 0xA7

ioctlGetProperty :: MonadInIO m => StructGetProperty -> Handle -> Excepts '[ErrorCode] m StructGetProperty
ioctlGetProperty = drmIoctl 0xAA

ioctlSetProperty :: MonadInIO m => StructSetProperty -> Handle -> Excepts '[ErrorCode] m StructSetProperty
ioctlSetProperty = drmIoctl 0xAB

ioctlGetBlob :: MonadInIO m => StructGetBlob -> Handle -> Excepts '[ErrorCode] m StructGetBlob
ioctlGetBlob = drmIoctl 0xAC

ioctlRemoveFrame :: MonadInIO m => Word32 -> Handle -> Excepts '[ErrorCode] m Word32
ioctlRemoveFrame = drmIoctl 0xAF

ioctlSwitchFrame :: MonadInIO m => StructSwitchFrame -> Handle -> Excepts '[ErrorCode] m StructSwitchFrame
ioctlSwitchFrame = drmIoctl 0xB0

ioctlDirtyFrame :: MonadInIO m => StructFrameDirty -> Handle -> Excepts '[ErrorCode] m StructFrameDirty
ioctlDirtyFrame = drmIoctl 0xB1

ioctlCreateHostBuffer :: MonadInIO m => StructCreateDumb -> Handle -> Excepts '[ErrorCode] m StructCreateDumb
ioctlCreateHostBuffer = drmIoctl 0xB2

ioctlMapHostBuffer :: MonadInIO m => StructMapDumb -> Handle -> Excepts '[ErrorCode] m StructMapDumb
ioctlMapHostBuffer = drmIoctl 0xB3

ioctlDestroyHostBuffer :: MonadInIO m => StructDestroyDumb -> Handle -> Excepts '[ErrorCode] m StructDestroyDumb
ioctlDestroyHostBuffer = drmIoctl 0xB4

ioctlGetPlaneResources :: MonadInIO m => StructGetPlaneRes -> Handle -> Excepts '[ErrorCode] m StructGetPlaneRes
ioctlGetPlaneResources = drmIoctl 0xB5

ioctlGetPlane :: MonadInIO m => StructGetPlane -> Handle -> Excepts '[ErrorCode] m StructGetPlane
ioctlGetPlane = drmIoctl 0xB6

ioctlSetPlane :: MonadInIO m => StructSetPlane -> Handle -> Excepts '[ErrorCode] m StructSetPlane
ioctlSetPlane = drmIoctl 0xB7

ioctlAddFrame :: MonadInIO m => StructFrameCommand -> Handle -> Excepts '[ErrorCode] m StructFrameCommand
ioctlAddFrame = drmIoctl 0xB8

ioctlGetObjectProperties :: MonadInIO m => StructGetObjectProperties -> Handle -> Excepts '[ErrorCode] m StructGetObjectProperties
ioctlGetObjectProperties = drmIoctl 0xB9

ioctlSetObjectProperty :: MonadInIO m => StructSetObjectProperty -> Handle -> Excepts '[ErrorCode] m StructSetObjectProperty
ioctlSetObjectProperty = drmIoctl 0xBA

ioctlCursor :: MonadInIO m => StructCursor2 -> Handle -> Excepts '[ErrorCode] m StructCursor2
ioctlCursor = drmIoctl 0xBB

ioctlAtomic :: MonadInIO m => StructAtomic -> Handle -> Excepts '[ErrorCode] m StructAtomic
ioctlAtomic = drmIoctl 0xBC

ioctlCreateBlob :: MonadInIO m => StructCreateBlob -> Handle -> Excepts '[ErrorCode] m StructCreateBlob
ioctlCreateBlob = drmIoctl 0xBD

ioctlDestroyBlob :: MonadInIO m => StructDestroyBlob -> Handle -> Excepts '[ErrorCode] m StructDestroyBlob
ioctlDestroyBlob = drmIoctl 0xBE



-----------------------------------------------------------------------------
-- Events
-----------------------------------------------------------------------------

-- Header for events written back to userspace on the drm fd.  The
-- type defines the type of event, the length specifies the total
-- length of the event (including the header), and user_data is
-- typically a 64 bit value passed with the ioctl that triggered the
-- event.  A read on the drm fd will always only return complete
-- events, that is, if for example the read buffer is 100 bytes, and
-- there are two 64 byte events pending, only one will be returned.
-- 
-- Event types 0 - 0x7fffffff are generic drm events, 0x80000000 and
-- up are chipset specific.

-- | drm_event
data EventHeader = EventHeader
   { eventType     :: {-# UNPACK #-} !Word32
   , eventLength   :: {-# UNPACK #-} !Word32
   } deriving (Show,Generic,Storable)

-- | Event type
data EventType
   = VBlankStartEvent        -- ^ Beginning of the VBlank period
   | FrameSwitchedEvent      -- ^ Frame switching complete
   | SequenceReachedEvent    -- ^ Controller sequence event
   | CustomEventType Word32  -- ^ Custom event
   deriving (Eq,Ord,Show)

-- | Try to recognize the event type
toEventType :: Word32 -> EventType
toEventType v = case v of
   0x01 -> VBlankStartEvent
   0x02 -> FrameSwitchedEvent
   0x03 -> SequenceReachedEvent
   _    -> CustomEventType v

-- | Event data
--
-- This is used both for VBlankStart and FrameSwitched events
--
-- drm_event_vblank (without the header)
data EventData = EventData
   { eventUserData     :: {-# UNPACK #-} !Word64
   , eventSeconds      :: {-# UNPACK #-} !Word32
   , eventMicroseconds :: {-# UNPACK #-} !Word32
   , eventSequence     :: {-# UNPACK #-} !Word32
   , eventControllerId :: {-# UNPACK #-} !Word32
   } deriving (Show,Generic,Storable)

-- | Sequence event data
--
-- Event delivered at sequence. Time stamp marks when the first pixel
-- of the refresh cycle leaves the display engine for the display
--
-- drm_event_crtc_sequence (withtout the header)
data SequenceEventData = SequenceEventData
   { sequenceEventUserData     :: {-# UNPACK #-} !Word64
   , sequenceEventTimestampNS  :: {-# UNPACK #-} !Int64 -- ^ Timestamp: first pixel leaves the display engine for the display (in nanoseconsd)
   , sequenceEventSequence     :: {-# UNPACK #-} !Word64
   } deriving (Show,Generic,Storable)

-- =============================================================
--    From linux/include/uapi/drm/drm_crtc.h
-- =============================================================

-----------------------------------------------------------------------------
-- SubPixel order
-----------------------------------------------------------------------------

-- | Indicate how a pixel is physically subdivised in RGB pixel elements
data SubPixel
   = SubPixelUnknown
   | SubPixelHorizontalRGB
   | SubPixelHorizontalBGR
   | SubPixelVerticalRGB
   | SubPixelVerticalBGR
   | SubPixelNone
   deriving (Eq,Ord,Enum,Show,CEnum)

