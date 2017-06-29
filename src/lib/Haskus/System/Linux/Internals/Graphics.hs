{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- | DRM/KMS Internals
--
-- Bindings with C structures and IOCTLs
module Haskus.System.Linux.Internals.Graphics
   (
   -- * Mode
     ModeType (..)
   , ModeTypes
   , ModeFlag (..)
   , ModeFlags
   , Stereo3D (..)
   , ModeFlagsStereo3D
   , PowerState(..)
   , ScalingMode(..)
   , AspectMode(..)
   , DitheringMode(..)
   , DirtyMode(..)
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
   , isPending
   , isImmutable
   , isAtomic
   , StructPropertyEnum (..)
   , StructGetProperty (..)
   , StructSetProperty (..)
   , StructGetObjectProperties (..)
   , StructSetObjectProperty (..)
   , StructGetBlob (..)
   -- * Framebuffer
   , FrameBufferFlag (..)
   , FrameBufferFlags
   , StructFrameBufferCommand (..)
   , DirtyAnnotation (..)
   , dirtyMaxClips
   , StructFrameBufferDirty (..)
   , StructModeCommand (..)   -- move
   -- * Cursor
   , CursorFlag (..)
   , CursorFlags
   , StructCursor (..)
   , StructCursor2 (..)
   -- * Gamma look-up table
   , StructControllerLut (..)
   -- * Page flipping
   , PageFlipFlag (..)
   , PageFlipFlags
   , StructPageFlip (..)
   , StructPageFlipTarget (..)
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
   , ioctlPageFlip
   , ioctlDirtyFrameBuffer
   , ioctlCreateGenericBuffer
   , ioctlMapGenericBuffer
   , ioctlDestroyGenericBuffer
   , ioctlGetPlaneResources
   , ioctlGetPlane
   , ioctlSetPlane
   , ioctlAddFrameBuffer
   , ioctlRemoveFrameBuffer
   , ioctlCursor
   , ioctlAtomic
   , ioctlCreateBlob
   , ioctlDestroyBlob
   -- * Events
   , DRMEventHeader (..)
   , EventType (..)
   , toEventType
   , DRMEvent (..)
   -- * Rotation/reflection
   , Rotation (..)
   , Reflection (..)
   , RotateReflect
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
import Haskus.Utils.Types.Generics (Generic)

-- =============================================================
--    From linux/include/uapi/drm/drm_mode.h
-- =============================================================

-----------------------------------------------------------------------------
-- Mode
-----------------------------------------------------------------------------

-- | Mode type
data ModeType
   = ModeTypeBuiltin
   | ModeTypeClockC
   | ModeTypeControllerC
   | ModeTypePreferred
   | ModeTypeDefault
   | ModeTypeUserDef
   | ModeTypeDriver
   deriving (Show,Enum,CBitSet)

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
   | ModeFlagBroadCast
   | ModeFlagPixMux
   | ModeFlagDoubleClock
   | ModeFlagClockDiv2
   deriving (Show,Enum,CBitSet)

type ModeFlags = BitSet Word32 ModeFlag

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

type ModeFlagsStereo3D = BitFields Word32
   '[ BitField 18 "stereo3d" (EnumField Word32 Stereo3D)
    , BitField 14 "flags"    ModeFlags
    ]

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

-- | Aspect mode
data AspectMode
   = AspectNone
   | Aspect4_3
   | Aspect16_9
   deriving(Show,Eq,Enum)

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
   , miFlags      :: {-# UNPACK #-} !ModeFlagsStereo3D
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
   deriving (Show,Enum,CBitSet)

type ModeFieldPresents = BitSet Word32 ModeFieldPresent

-- | drm_mode_set_plane
--
-- Planes blend with or override other bits on the CRTC
data StructSetPlane = StructSetPlane
   { spPlaneId :: {-# UNPACK #-} !Word32
   , spCrtcId  :: {-# UNPACK #-} !Word32
   , spFbId    :: {-# UNPACK #-} !Word32 -- ^ Frame buffer contains surface format type
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

isPending :: StructGetProperty -> Bool
isPending x = testBit (gpsFlags x) 0

isImmutable :: StructGetProperty -> Bool
isImmutable x = testBit (gpsFlags x) 2

isAtomic :: StructGetProperty -> Bool
isAtomic x = testBit (gpsFlags x) 31

-- | drm_mode_property_enum
data StructPropertyEnum = StructPropertyEnum
   { peValue       :: {-# UNPACK #-} !Word64
   , peName        :: {-# UNPACK #-} !(CStringBuffer 32)
   } deriving (Generic,Storable)

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
-- Framebuffer
-----------------------------------------------------------------------------

-- we don't use drm_mode_fb_cmd as we have drm_mode_fb_cmd2

-- | Frame buffer flags
data FrameBufferFlag
   = FrameBufferInterlaced    -- ^ Interlaced frame buffer
   | FrameBufferUseModifiers  -- ^ Enable modifiers
   deriving (Show,Eq,Enum,CBitSet)

type FrameBufferFlags = BitSet Word32 FrameBufferFlag

-- | Data matching the C structure drm_mode_fb_cmd2
data StructFrameBufferCommand = StructFrameBufferCommand
   { fc2FbId          :: {-# UNPACK #-} !Word32
   , fc2Width         :: {-# UNPACK #-} !Word32
   , fc2Height        :: {-# UNPACK #-} !Word32
   , fc2PixelFormat   :: {-# UNPACK #-} !PixelFormat
   , fc2Flags         :: {-# UNPACK #-} !FrameBufferFlags
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
data StructFrameBufferDirty = StructFrameBufferDirty
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
   deriving (Eq,Enum,Show,CBitSet)

type CursorFlags = BitSet Word32 CursorFlag

-- | drm_mode_cursor
data StructCursor = StructCursor
   { curFlags     :: {-# UNPACK #-} !CursorFlags
   , curCrtcId    :: {-# UNPACK #-} !Word32
   , curX         :: {-# UNPACK #-} !Int32
   , curY         :: {-# UNPACK #-} !Int32
   , curWidth     :: {-# UNPACK #-} !Word32
   , curHeight    :: {-# UNPACK #-} !Word32
   , curHandle    :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)

-- | drm_mode_cursor2
data StructCursor2 = StructCursor2
   { cur2Flags     :: {-# UNPACK #-} !CursorFlags
   , cur2CrtcId    :: {-# UNPACK #-} !Word32
   , cur2X         :: {-# UNPACK #-} !Int32
   , cur2Y         :: {-# UNPACK #-} !Int32
   , cur2Width     :: {-# UNPACK #-} !Word32
   , cur2Height    :: {-# UNPACK #-} !Word32
   , cur2Handle    :: {-# UNPACK #-} !Word32
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
   , clsRed          :: {-# UNPACK #-} !Word64
   , clsGreen        :: {-# UNPACK #-} !Word64
   , clsBlue         :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

-----------------------------------------------------------------------------
-- Page flipping
-----------------------------------------------------------------------------

-- | Page flip flags
data PageFlipFlag
   = PageFlipEvent
   | PageFlipAsync
   | PageFlipTargetAbsolute
   | PageFlipTargetRelative
   deriving (Show,Eq,Enum,CBitSet)

type PageFlipFlags = BitSet Word32 PageFlipFlag

-- 
-- Request a page flip on the specified crtc.
-- 
-- This ioctl will ask KMS to schedule a page flip for the specified
-- crtc.  Once any pending rendering targeting the specified fb (as of
-- ioctl time) has completed, the crtc will be reprogrammed to display
-- that fb after the next vertical refresh.  The ioctl returns
-- immediately, but subsequent rendering to the current fb will block
-- in the execbuffer ioctl until the page flip happens.  If a page
-- flip is already pending as the ioctl is called, EBUSY will be
-- returned.
-- 
-- Flag DRM_MODE_PAGE_FLIP_EVENT requests that drm sends back a vblank
-- event (see drm.h: struct drm_event_vblank) when the page flip is
-- done.  The user_data field passed in with this ioctl will be
-- returned as the user_data field in the vblank event struct.
-- 
-- Flag DRM_MODE_PAGE_FLIP_ASYNC requests that the flip happen
-- 'as soon as possible', meaning that it not delay waiting for vblank.
-- This may cause tearing on the screen.
-- 
-- The reserved field must be zero.

-- | drm_mode_crtc_page_flip
data StructPageFlip = StructPageFlip
   { pfCrtcId        :: {-# UNPACK #-} !Word32
   , pfFbId          :: {-# UNPACK #-} !Word32
   , pfFlags         :: {-# UNPACK #-} !PageFlipFlags
   , pfReserved      :: {-# UNPACK #-} !Word32
   , pfUserData      :: {-# UNPACK #-} !Word64
   } deriving (Generic,Storable)

--
-- Request a page flip on the specified crtc.
--
-- Same as struct drm_mode_crtc_page_flip, but supports new flags and
-- re-purposes the reserved field:
--
-- The sequence field must be zero unless either of the
-- DRM_MODE_PAGE_FLIP_TARGET_ABSOLUTE/RELATIVE flags is specified. When
-- the ABSOLUTE flag is specified, the sequence field denotes the absolute
-- vblank sequence when the flip should take effect. When the RELATIVE
-- flag is specified, the sequence field denotes the relative (to the
-- current one when the ioctl is called) vblank sequence when the flip
-- should take effect. NOTE: DRM_IOCTL_WAIT_VBLANK must still be used to
-- make sure the vblank sequence before the target one has passed before
-- calling this ioctl. The purpose of the
-- DRM_MODE_PAGE_FLIP_TARGET_ABSOLUTE/RELATIVE flags is merely to clarify
-- the target for when code dealing with a page flip runs during a
-- vertical blank period.

-- drm_mode_crtc_page_flip_target
data StructPageFlipTarget = StructPageFlipTarget
   { pftCrtcId   :: {-# UNPACK #-} !Word32
   , pftFbId     :: {-# UNPACK #-} !Word32
   , pftFlags    :: {-# UNPACK #-} !Word32
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
data AtomicFlag
   = AtomicFlagPageFlipEvent  -- ^ Generates a page-flip event
   | AtomicFlagPageFlipAsync  -- ^ Asynchronous page-flip, i.e. don't wait for v-blank (may not be supported)
   | AtomicFlagTestOnly       -- ^ Only test the config, don't commit it
   | AtomicFlagNonBlock       -- ^ Schedule an asynchronous commit (may not be supported)
   | AtomicFlagAllowModeset   -- ^ Allow full mode-setting. This flag is useful for devices such as tablets whose screen is often shutdown: we can use a degraded mode (scaled, etc.) for a while to save power and only perform the full modeset when the screen is reactivated.
   deriving (Show,Eq,Enum)

instance CBitSet AtomicFlag where
   toBitOffset x = case x of
      AtomicFlagPageFlipEvent -> 0
      AtomicFlagPageFlipAsync -> 1
      AtomicFlagTestOnly      -> 8
      AtomicFlagNonBlock      -> 9
      AtomicFlagAllowModeset  -> 10
   fromBitOffset x = case x of
      0  -> AtomicFlagPageFlipEvent
      1  -> AtomicFlagPageFlipAsync
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
   = CapGenericBuffer         -- ^ Support generic buffers (i.e. not vendor specific)
   | CapVBlankHighController
   | CapGenericPreferredDepth
   | CapGenericPreferShadow
   | CapPrime
   | CapTimestampMonotonic
   | CapAsyncPageFlip         -- ^ Support asynchronous page-flipping
   | CapCursorWidth
   | CapCursorHeight
   | CapAddFrameBufferModifiers
   | CapPageFlipTarget
   deriving (Show,Eq,Enum)

-- Add 1 to the enum number to get the valid value
instance CEnum Capability where
   fromCEnum = (+1) . fromIntegral . fromEnum
   toCEnum   = toEnum . (\x -> x-1) . fromIntegral

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
   = ClientCapStereo3D        -- ^ if set, the DRM core will expose the stereo 3D capabilities of the monitor by advertising the supported 3D layouts in the flags of struct drm_mode_modeinfo (cf Stereo3D)
   | ClientCapUniversalPlanes -- ^ If set, the DRM core will expose all planes (overlay, primary, and cursor) to userspace.
   | ClientCapAtomic          -- ^ If set, the DRM core will expose atomic properties to userspace
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
   deriving (Show,Eq,CBitSet)

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

drmIoctl :: Storable a => Word8 -> a -> Handle -> IOErr a
drmIoctl = ioctlWriteRead 0x64


ioctlGetCapabilities :: StructGetCap -> Handle -> IOErr StructGetCap
ioctlGetCapabilities = drmIoctl 0x0C

ioctlSetClientCapability :: StructSetClientCap -> Handle -> IOErr StructSetClientCap
ioctlSetClientCapability = drmIoctl 0x0D
 
 
ioctlGetResources :: StructCardRes -> Handle -> IOErr StructCardRes
ioctlGetResources = drmIoctl 0xA0

ioctlGetController :: StructController -> Handle -> IOErr StructController
ioctlGetController = drmIoctl 0xA1

ioctlSetController :: StructController -> Handle -> IOErr StructController
ioctlSetController = drmIoctl 0xA2

ioctlGetGamma :: StructControllerLut -> Handle -> IOErr StructControllerLut
ioctlGetGamma = drmIoctl 0xA4

ioctlSetGamma :: StructControllerLut -> Handle -> IOErr StructControllerLut
ioctlSetGamma = drmIoctl 0xA5

ioctlGetEncoder :: StructGetEncoder -> Handle -> IOErr StructGetEncoder
ioctlGetEncoder = drmIoctl 0xA6

ioctlGetConnector :: StructGetConnector -> Handle -> IOErr StructGetConnector
ioctlGetConnector = drmIoctl 0xA7

ioctlGetProperty :: StructGetProperty -> Handle -> IOErr StructGetProperty
ioctlGetProperty = drmIoctl 0xAA

ioctlSetProperty :: StructSetProperty -> Handle -> IOErr StructSetProperty
ioctlSetProperty = drmIoctl 0xAB

ioctlGetBlob :: StructGetBlob -> Handle -> IOErr StructGetBlob
ioctlGetBlob = drmIoctl 0xAC

ioctlRemoveFrameBuffer :: Word32 -> Handle -> IOErr Word32
ioctlRemoveFrameBuffer = drmIoctl 0xAF

ioctlPageFlip :: StructPageFlip -> Handle -> IOErr StructPageFlip
ioctlPageFlip = drmIoctl 0xB0

ioctlDirtyFrameBuffer :: StructFrameBufferDirty -> Handle -> IOErr StructFrameBufferDirty
ioctlDirtyFrameBuffer = drmIoctl 0xB1

ioctlCreateGenericBuffer :: StructCreateDumb -> Handle -> IOErr StructCreateDumb
ioctlCreateGenericBuffer = drmIoctl 0xB2

ioctlMapGenericBuffer :: StructMapDumb -> Handle -> IOErr StructMapDumb
ioctlMapGenericBuffer = drmIoctl 0xB3

ioctlDestroyGenericBuffer :: StructDestroyDumb -> Handle -> IOErr StructDestroyDumb
ioctlDestroyGenericBuffer = drmIoctl 0xB4

ioctlGetPlaneResources :: StructGetPlaneRes -> Handle -> IOErr StructGetPlaneRes
ioctlGetPlaneResources = drmIoctl 0xB5

ioctlGetPlane :: StructGetPlane -> Handle -> IOErr StructGetPlane
ioctlGetPlane = drmIoctl 0xB6

ioctlSetPlane :: StructSetPlane -> Handle -> IOErr StructSetPlane
ioctlSetPlane = drmIoctl 0xB7

ioctlAddFrameBuffer :: StructFrameBufferCommand -> Handle -> IOErr StructFrameBufferCommand
ioctlAddFrameBuffer = drmIoctl 0xB8

ioctlGetObjectProperties :: StructGetObjectProperties -> Handle -> IOErr StructGetObjectProperties
ioctlGetObjectProperties = drmIoctl 0xB9

ioctlSetObjectProperty :: StructSetObjectProperty -> Handle -> IOErr StructSetObjectProperty
ioctlSetObjectProperty = drmIoctl 0xBA

ioctlCursor :: StructCursor2 -> Handle -> IOErr StructCursor2
ioctlCursor = drmIoctl 0xBB

ioctlAtomic :: StructAtomic -> Handle -> IOErr StructAtomic
ioctlAtomic = drmIoctl 0xBC

ioctlCreateBlob :: StructCreateBlob -> Handle -> IOErr StructCreateBlob
ioctlCreateBlob = drmIoctl 0xBD

ioctlDestroyBlob :: StructDestroyBlob -> Handle -> IOErr StructDestroyBlob
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
data DRMEventHeader = DRMEventHeader
   { eventType     :: {-# UNPACK #-} !Word32
   , eventLength   :: {-# UNPACK #-} !Word32
   } deriving (Generic,Storable)

-- | Event type
data EventType
   = VBlank           -- ^ Beginning of the VBlank period
   | PageFlipComplete -- ^ Page flipping complete
   deriving (Show)

-- | Try to recognize the event type
toEventType :: Word32 -> Maybe EventType
toEventType v = case v of
   0x01 -> Just VBlank
   0x02 -> Just PageFlipComplete
   _    -> Nothing

-- | drm_event_vblank
data DRMEvent = DRMEvent
   { drmEventType         :: {-# UNPACK #-} !Word32
   , drmEventSize         :: {-# UNPACK #-} !Word32
   , drmEventUserData     :: {-# UNPACK #-} !Word64
   , drmEventSeconds      :: {-# UNPACK #-} !Word32
   , drmEventMicroseconds :: {-# UNPACK #-} !Word32
   , drmEventSequence     :: {-# UNPACK #-} !Word32
   , drmEventReserved     :: {-# UNPACK #-} !Word32
   } deriving (Show,Generic,Storable)

-- =============================================================
--    From linux/include/uapi/drm/drm_crtc.h
-- =============================================================

-----------------------------------------------------------------------------
-- Rotation/reflection
-----------------------------------------------------------------------------

data Rotation
   = RotateNone
   | Rotate90
   | Rotate180
   | Rotate270
   deriving (Show,Eq,Enum)

data Reflection
   = ReflectX
   | ReflectY
   deriving (Show,Eq,Enum,CBitSet)

type RotateReflect = BitFields Word8
   '[ BitField 2 "padding"    Word8
    , BitField 2 "reflection" (BitSet Word8 Reflection)
    , BitField 4 "rotation"   (EnumField Word8 Rotation)
    ]

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

