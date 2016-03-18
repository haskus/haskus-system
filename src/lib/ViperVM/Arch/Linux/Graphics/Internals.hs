{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

-- | DRM/KMS Internals
--
-- Bindings with C structures and IOCTLs
module ViperVM.Arch.Linux.Graphics.Internals
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
   , StructEvent (..)
   , EventType (..)
   , toEventType
   , StructEventVBlank (..)
   -- * Rotation/reflection
   , Rotation (..)
   , Reflection (..)
   , RotateReflect
   -- * SubPixel order
   , SubPixel (..)
   )
where

import ViperVM.Arch.Linux.Ioctl
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Vector as Vector
import ViperVM.Format.Binary.BitField
import ViperVM.Format.Binary.Enum

import ViperVM.Arch.Linux.Graphics.PixelFormat

import Foreign.Storable
import Foreign.CStorable
import Foreign.C.Types
import Foreign.C.String (castCharToCChar)
import Data.Word
import Data.Int
import Data.Bits
import GHC.Generics (Generic)

-- =============================================================
--    From linux/include/uapi/drm/drm_mode.h
-- =============================================================

-----------------------------------------------------------------------------
-- Mode
-----------------------------------------------------------------------------

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
   { miClock      :: Word32
   , miHDisplay   :: Word16
   , miHSyncStart :: Word16
   , miHSyncEnd   :: Word16
   , miHTotal     :: Word16
   , miHSkew      :: Word16
   , miVDisplay   :: Word16
   , miVSyncStart :: Word16
   , miVSyncEnd   :: Word16
   , miVTotal     :: Word16
   , miVScan      :: Word16
   , miVRefresh   :: Word32
   , miFlags      :: ModeFlagsStereo3D
   , miType       :: ModeTypes
   , miName       :: Vector 32 CChar
   } deriving (Generic)

instance CStorable StructMode

instance Storable StructMode where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

emptyStructMode :: StructMode
emptyStructMode = StructMode 0 0 0 0 0 0 0 0 0 0 0 0 (BitFields 0) BitSet.empty (Vector.replicate (castCharToCChar '\0'))

-----------------------------------------------------------------------------
-- Resources
-----------------------------------------------------------------------------

-- | drm_mode_card_res
data StructCardRes = StructCardRes
   { csFbIdPtr    :: Word64
   , csCrtcIdPtr  :: Word64
   , csConnIdPtr  :: Word64
   , csEncIdPtr   :: Word64
   , csCountFbs   :: Word32
   , csCountCrtcs :: Word32
   , csCountConns :: Word32
   , csCountEncs  :: Word32
   , csMinWidth   :: Word32
   , csMaxWidth   :: Word32
   , csMinHeight  :: Word32
   , csMaxHeight  :: Word32
   } deriving (Generic,CStorable)

instance Storable StructCardRes where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-----------------------------------------------------------------------------
-- Controller
-----------------------------------------------------------------------------

-- | drm_mode_crtc
data StructController = StructController
   { contSetConnPtr :: Word64
   , contConnCount  :: Word32
   , contID         :: Word32
   , contFbID       :: Word32
   , contFbX        :: Word32
   , contFbY        :: Word32
   , contGammaSize  :: Word32
   , contModeValid  :: Word32
   , contModeInfo   :: StructMode
   } deriving (Generic,CStorable)

instance Storable StructController where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek



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
   { spPlaneId :: Word32
   , spCrtcId  :: Word32
   , spFbId    :: Word32            -- ^ Frame buffer contains surface format type
   , spFlags   :: ModeFieldPresents
   , spCrtcX   :: Int32

   , spCrtcY   :: Int32             -- ^ Signed dest location allows it to be partially off screen
   , spCrtcW   :: Word32
   , spCrtcH   :: Word32

   , spSrcX    :: Word32            -- ^ Source values are 16.16 fixed point
   , spSrcY    :: Word32
   , spSrcH    :: Word32
   , spSrcW    :: Word32
   } deriving (Generic,CStorable)

instance Storable StructSetPlane where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | drm_mode_get_plane
data StructGetPlane = StructGetPlane
   { gpPlaneId       :: Word32
   , gpCrtcId        :: Word32
   , gpFbId          :: Word32
   , gpPossibleCrtcs :: Word32
   , gpGammaSize     :: Word32
   , gpCountFmtTypes :: Word32
   , gpFormatTypePtr :: Word64
   } deriving (Generic,CStorable)

instance Storable StructGetPlane where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

-- | drm_mode_get_plane_res
data StructGetPlaneRes = StructGetPlaneRes
   { gprsPlaneIdPtr  :: Word64
   , gprsCountPlanes :: Word32
   } deriving (Generic,CStorable)

instance Storable StructGetPlaneRes where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek

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
   { geEncoderId      :: Word32
   , geEncoderType    :: EnumField Word32 EncoderType
   , geCrtcId         :: Word32
   , gePossibleCrtcs  :: BitSet Word32 Int -- ^ Valid controller indexes
   , gePossibleClones :: BitSet Word32 Int -- ^ Valid clone encoder indexes
   } deriving (Generic,CStorable)

instance Storable StructGetEncoder where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   poke        = cPoke
   peek        = cPeek


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
   { connEncodersPtr       :: Word64
   , connModesPtr          :: Word64
   , connPropsPtr          :: Word64
   , connPropValuesPtr     :: Word64

   , connModesCount        :: Word32
   , connPropsCount        :: Word32
   , connEncodersCount     :: Word32

   , connEncoderID_        :: Word32   -- ^ current encoder
   , connConnectorID_      :: Word32   -- ^ ID
   , connConnectorType_    :: EnumField Word32 ConnectorType
   , connConnectorTypeID_  :: Word32

   , connConnection_       :: Word32
   , connWidth_            :: Word32   -- ^ HxW in millimeters
   , connHeight_           :: Word32
   , connSubPixel_         :: EnumField Word32 SubPixel
   } deriving (Generic,CStorable)

instance Storable StructGetConnector where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

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
   { peValue       :: Word64
   , peName        :: Vector 32 CChar
   } deriving (Generic,CStorable)

instance Storable StructPropertyEnum where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | drm_mode_get_property
data StructGetProperty = StructGetProperty
   { gpsValuesPtr      :: Word64 -- ^ Values or blob lengths
   , gpsEnumBlobPtr    :: Word64 -- ^ Enum or blob id ptrs
   , gpsPropId         :: Word32
   , gpsFlags          :: Word32
   , gpsName           :: Vector 32 CChar
   , gpsCountValues    :: Word32
   , gpsCountEnum      :: Word32
   } deriving (Generic,CStorable)

instance Storable StructGetProperty where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | drm_mode_set_property
data StructSetProperty = StructSetProperty
   { spsValue        :: Word64
   , spsPropId       :: Word32
   , spsConnId       :: Word32
   } deriving (Generic,CStorable)

instance Storable StructSetProperty where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | drm_mode_obj_get_properties
data StructGetObjectProperties = StructGetObjectProperties
   { gopPropsPtr        :: Word64
   , gopValuesPtr       :: Word64
   , gopCountProps      :: Word32
   , gopObjId           :: Word32
   , gopObjType         :: Word32
   } deriving (Generic,CStorable)

instance Storable StructGetObjectProperties where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | drm_mode_obj_set_property
data StructSetObjectProperty = StructSetObjectProperty
   { sopValue           :: Word64
   , sopPropId          :: Word32
   , sopObjId           :: Word32
   , sopObjType         :: Word32
   } deriving (Generic,CStorable)

instance Storable StructSetObjectProperty where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | drm_mode_get_blob
data StructGetBlob = StructGetBlob
   { gbBlobId     :: Word32
   , gbLength     :: Word32
   , gbData       :: Word64
   } deriving (Generic,CStorable)

instance Storable StructGetBlob where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

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
   { fc2FbId          :: Word32
   , fc2Width         :: Word32
   , fc2Height        :: Word32
   , fc2PixelFormat   :: PixelFormat
   , fc2Flags         :: FrameBufferFlags
   , fc2Handles       :: Vector 4 Word32
   , fc2Pitches       :: Vector 4 Word32  -- ^ Pitch for each plane
   , fc2Offsets       :: Vector 4 Word32  -- ^ Offset of each plane
   , fc2Modifiers     :: Vector 4 Word64  -- ^ tiling, compressed
   } deriving (Generic,CStorable)

instance Storable StructFrameBufferCommand where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

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
   { fdFbId          :: Word32
   , fdFlags         :: Word32
   , fdColor         :: Word32
   , fdNumClips      :: Word32
   , fdClipsPtr      :: Word64
   } deriving (Generic,CStorable)

instance Storable StructFrameBufferDirty where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | drm_mode_mode_cmd
data StructModeCommand = StructModeCommand
   { mcConnId     :: Word32
   , mcMode       :: StructMode
   } deriving (Generic,CStorable)

instance Storable  StructModeCommand where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


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
   { curFlags     :: CursorFlags
   , curCrtcId    :: Word32
   , curX         :: Int32
   , curY         :: Int32
   , curWidth     :: Word32
   , curHeight    :: Word32
   , curHandle    :: Word32
   } deriving (Generic,CStorable)

instance Storable  StructCursor where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- | drm_mode_cursor2
data StructCursor2 = StructCursor2
   { cur2Flags     :: CursorFlags
   , cur2CrtcId    :: Word32
   , cur2X         :: Int32
   , cur2Y         :: Int32
   , cur2Width     :: Word32
   , cur2Height    :: Word32
   , cur2Handle    :: Word32
   , cur2HotX      :: Int32
   , cur2HotY      :: Int32
   } deriving (Generic,CStorable)

instance Storable  StructCursor2 where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-----------------------------------------------------------------------------
-- Gamma look-up table
-----------------------------------------------------------------------------

-- | drm_mode_crtc_lut
data StructControllerLut = StructControllerLut
   { clsCrtcId       :: Word32
   , clsGammaSize    :: Word32
   , clsRed          :: Word64
   , clsGreen        :: Word64
   , clsBlue         :: Word64
   } deriving (Generic,CStorable)

instance Storable StructControllerLut where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-----------------------------------------------------------------------------
-- Page flipping
-----------------------------------------------------------------------------

data PageFlipFlag
   = PageFlipEvent
   | PageFlipAsync
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
-- The reserved field must be zero until we figure out something
-- clever to use it for.

-- | drm_mode_crtc_page_flip
data StructPageFlip = StructPageFlip
   { pfCrtcId        :: Word32
   , pfFbId          :: Word32
   , pfFlags         :: PageFlipFlags
   , pfReserved      :: Word32
   , pfUserData      :: Word64
   } deriving (Generic,CStorable)

instance Storable  StructPageFlip where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-----------------------------------------------------------------------------
-- Generic buffer
-----------------------------------------------------------------------------

-- | drm_mode_create_dumb
data StructCreateDumb = StructCreateDumb
   { cdHeight :: Word32
   , cdWidth  :: Word32
   , cdBPP    :: Word32 -- ^ Bits per pixel
   , cdFlags  :: Word32
   , cdHandle :: Word32 -- ^ Handle, pitch, size will be returned
   , cdPitch  :: Word32
   , cdSize   :: Word64
   } deriving (Show,Generic,CStorable)

instance Storable  StructCreateDumb where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | drm_mode_map_dumb
data StructMapDumb = StructMapDumb
   { mdHandle :: Word32
   , mdPad    :: Word32  -- Padding field: not useful
   , mdOffset :: Word64  -- ^ Fake offset to use for subsequent mmap call
   } deriving (Show,Generic,CStorable)

instance Storable StructMapDumb where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | drm_mode_destroy_dumb
data StructDestroyDumb = StructDestroyDumb
   { dbHanlde :: Word32
   } deriving (Generic,CStorable)

instance Storable StructDestroyDumb where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-----------------------------------------------------------------------------
-- Atomic
-----------------------------------------------------------------------------

data AtomicFlag
   = AtomicFlagPageFlipEvent
   | AtomicFlagPageFlipAsync
   | AtomicFlagTestOnly
   | AtomicFlagNonBlock
   | AtomicFlagAllowModeset
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

type AtomicFlags = BitSet Word32 AtomicFlag

-- | drm_mode_atomic
data StructAtomic = StructAtomic
   { atomFlags         :: AtomicFlags
   , atomCountObjects  :: Word32
   , atomObjectsPtr    :: Word64
   , atomCountPropsPtr :: Word64
   , atomPropsPtr      :: Word64
   , atomPropValuesPtr :: Word64
   , atomReserved      :: Word64
   , atomUserData      :: Word64
   } deriving (Generic,CStorable)

instance Storable StructAtomic where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-----------------------------------------------------------------------------
-- Blob
-----------------------------------------------------------------------------

-- | Create a new 'blob' data property, copying length bytes from data pointer,
-- and returning new blob ID.
data StructCreateBlob = StructCreateBlob
   { cbData   :: Word64 -- ^ Pointer to data to copy
   , cbLength :: Word32 -- ^ Length of data to copy
   , cbBlobID :: Word32 -- ^ Return: new property ID
   } deriving (Generic,CStorable)

instance Storable StructCreateBlob where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


-- | Destroy a user-created blob property.
data StructDestroyBlob = StructDestroyBlob
   { dbBlobId :: Word32
   } deriving (Generic,CStorable)

instance Storable StructDestroyBlob where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

-- =============================================================
--    From linux/include/uapi/drm/drm.h
-- =============================================================

-----------------------------------------------------------------------------
-- Generic
-----------------------------------------------------------------------------

data Clip = Clip
   { clipX1 :: Word16
   , clipY1 :: Word16
   , clipX2 :: Word16
   , clipY2 :: Word16
   } deriving (Show,Eq,Generic,CStorable)

instance Storable Clip where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke


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
   { gcCapability :: EnumField Word64 Capability
   , gcValue      :: Word64
   } deriving (Generic,CStorable)

instance Storable StructGetCap where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

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
   { sccCapability :: EnumField Word64 ClientCapability
   , sccValue      :: Word64
   } deriving (Generic,CStorable)

instance Storable StructSetClientCap where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-----------------------------------------------------------------------------
-- IOCTLs
-----------------------------------------------------------------------------

drmIoctl :: Storable a => Word8 -> FileDescriptor -> a -> SysRet a
drmIoctl n = ioctlReadWrite sysIoctl 0x64 n defaultCheck


ioctlGetCapabilities :: FileDescriptor -> StructGetCap -> SysRet StructGetCap
ioctlGetCapabilities = drmIoctl 0x0C

ioctlSetClientCapability :: FileDescriptor -> StructSetClientCap -> SysRet StructSetClientCap
ioctlSetClientCapability = drmIoctl 0x0D
 
 
ioctlGetResources :: FileDescriptor -> StructCardRes -> SysRet StructCardRes
ioctlGetResources = drmIoctl 0xA0

ioctlGetController :: FileDescriptor -> StructController -> SysRet StructController
ioctlGetController = drmIoctl 0xA1

ioctlSetController :: FileDescriptor -> StructController -> SysRet StructController
ioctlSetController = drmIoctl 0xA2

ioctlGetGamma :: FileDescriptor -> StructControllerLut -> SysRet StructControllerLut
ioctlGetGamma = drmIoctl 0xA4

ioctlSetGamma :: FileDescriptor -> StructControllerLut -> SysRet StructControllerLut
ioctlSetGamma = drmIoctl 0xA5

ioctlGetEncoder :: FileDescriptor -> StructGetEncoder -> SysRet StructGetEncoder
ioctlGetEncoder = drmIoctl 0xA6

ioctlGetConnector :: FileDescriptor -> StructGetConnector -> SysRet StructGetConnector
ioctlGetConnector = drmIoctl 0xA7

ioctlGetProperty :: FileDescriptor -> StructGetProperty -> SysRet StructGetProperty
ioctlGetProperty = drmIoctl 0xAA

ioctlSetProperty :: FileDescriptor -> StructSetProperty -> SysRet StructSetProperty
ioctlSetProperty = drmIoctl 0xAB

ioctlGetBlob :: FileDescriptor -> StructGetBlob -> SysRet StructGetBlob
ioctlGetBlob = drmIoctl 0xAC

ioctlRemoveFrameBuffer :: FileDescriptor -> Word32 -> SysRet Word32
ioctlRemoveFrameBuffer = drmIoctl 0xAF

ioctlPageFlip :: FileDescriptor -> StructPageFlip -> SysRet StructPageFlip
ioctlPageFlip = drmIoctl 0xB0

ioctlDirtyFrameBuffer :: FileDescriptor -> StructFrameBufferDirty -> SysRet StructFrameBufferDirty
ioctlDirtyFrameBuffer = drmIoctl 0xB1

ioctlCreateGenericBuffer :: FileDescriptor -> StructCreateDumb -> SysRet StructCreateDumb
ioctlCreateGenericBuffer = drmIoctl 0xB2

ioctlMapGenericBuffer :: FileDescriptor -> StructMapDumb -> SysRet StructMapDumb
ioctlMapGenericBuffer = drmIoctl 0xB3

ioctlDestroyGenericBuffer :: FileDescriptor -> StructDestroyDumb -> SysRet StructDestroyDumb
ioctlDestroyGenericBuffer = drmIoctl 0xB4

ioctlGetPlaneResources :: FileDescriptor -> StructGetPlaneRes -> SysRet StructGetPlaneRes
ioctlGetPlaneResources = drmIoctl 0xB5

ioctlGetPlane :: FileDescriptor -> StructGetPlane -> SysRet StructGetPlane
ioctlGetPlane = drmIoctl 0xB6

ioctlSetPlane :: FileDescriptor -> StructSetPlane -> SysRet StructSetPlane
ioctlSetPlane = drmIoctl 0xB7

ioctlAddFrameBuffer :: FileDescriptor -> StructFrameBufferCommand -> SysRet StructFrameBufferCommand
ioctlAddFrameBuffer = drmIoctl 0xB8

ioctlGetObjectProperties :: FileDescriptor -> StructGetObjectProperties -> SysRet StructGetObjectProperties
ioctlGetObjectProperties = drmIoctl 0xB9

ioctlSetObjectProperty :: FileDescriptor -> StructSetObjectProperty -> SysRet StructSetObjectProperty
ioctlSetObjectProperty = drmIoctl 0xBA

ioctlCursor :: FileDescriptor -> StructCursor2 -> SysRet StructCursor2
ioctlCursor = drmIoctl 0xBB

ioctlAtomic :: FileDescriptor -> StructAtomic -> SysRet StructAtomic
ioctlAtomic = drmIoctl 0xBC

ioctlCreateBlob :: FileDescriptor -> StructCreateBlob -> SysRet StructCreateBlob
ioctlCreateBlob = drmIoctl 0xBD

ioctlDestroyBlob :: FileDescriptor -> StructDestroyBlob -> SysRet StructDestroyBlob
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
data StructEvent = StructEvent
   { eventType   :: Word32
   , eventLength :: Word32
   } deriving (Generic,CStorable)

instance Storable StructEvent  where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

data EventType
   = VBlank
   | FlipComplete
   deriving (Show)

-- | Try to recognize the event type
toEventType :: Word32 -> Maybe EventType
toEventType v = case v of
   0x01 -> Just VBlank
   0x02 -> Just FlipComplete
   _    -> Nothing

-- | drm_event_vblank
data StructEventVBlank = StructEventVBlank
   { vblankEventType         :: Word32
   , vblankEventSize         :: Word32
   , vblankEventUserData     :: Word64
   , vblankEventSeconds      :: Word32
   , vblankEventMicroseconds :: Word32
   , vblankEventSequence     :: Word32
--   , vblankEventReserved           :: Word32
   } 
   deriving (Show,Generic,CStorable)

instance Storable StructEventVBlank where
   sizeOf      = cSizeOf
   alignment   = cAlignment
   peek        = cPeek
   poke        = cPoke

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

