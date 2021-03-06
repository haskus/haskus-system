{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskus.System.Linux.Graphics.Entities
   ( -- * IDs
     EntityID (..)
   , FrameID
   , ControllerID
   , ConnectorID
   , EncoderID
   , PlaneID
   , BlobID
   , PropertyID
   , ObjectID
   , castEntityID
   -- * Connector
   , Connector (..)
   , Connection (..)
   , Display (..)
   , IsConnector (..)
   -- * Encoder
   , Encoder (..)
   , EncoderType (..)
   -- * Controller
   , Controller (..)
   , FrameView (..)
   , IsController (..)
   -- * Plane
   , Plane (..)
   , PlaneTarget (..)
   , PlaneSource (..)
   , IsPlane (..)
   , PlaneType (..)
   -- * Frame
   , Frame (..)
   , IsFrame (..)
   , FrameBuffer (..)
   , showFrame
   , showFrameBuffer
   , ShowBuffer(..)
   -- * Property
   , PropertyMeta (..)
   , Property (..)
   , PropertyType (..)
   , RawProperty (..)
   , FP16_16
   )
where

import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Storable
import Haskus.Number.FixedPoint
import Haskus.Binary.Buffer
import qualified Haskus.Binary.BitSet as BitSet
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Graphics.KIO
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.Utils.Flow
import Data.Coerce

-------------------------------------------------------------------------------
-- IDs
-------------------------------------------------------------------------------

type ObjectID = Word32

-- | Entity identifier
newtype EntityID a = EntityID
   { unEntityID :: ObjectID
   } deriving (Show,Eq,Storable,Ord)

type FrameID        = EntityID (Frame ())
type ConnectorID    = EntityID Connector
type ControllerID   = EntityID Controller
type EncoderID      = EntityID Encoder
type PlaneID        = EntityID Plane
type PropertyID     = EntityID PropertyMeta
type BlobID x       = EntityID x

-- | Cast an entity ID
castEntityID :: EntityID x -> EntityID y
castEntityID = coerce

-------------------------------------------------------------------------------
-- Connector
-------------------------------------------------------------------------------

-- | A connector on the graphic card
data Connector = Connector
   { connectorID                 :: ConnectorID        -- ^ Connector identifier
   , connectorType               :: ConnectorType      -- ^ Type of connector
   , connectorByTypeIndex        :: Word32             -- ^ Identifier within connectors of the same type
   , connectorState              :: Connection         -- ^ Connection state
   , connectorPossibleEncoderIDs :: [EncoderID]        -- ^ IDs of the encoders that can work with this connector
   , connectorEncoderID          :: Maybe EncoderID    -- ^ Currently used encoder
   , connectorControllerID       :: Maybe ControllerID -- ^ Current driving controller
   , connectorHandle             :: Handle             -- ^ Graphic card
   } deriving (Show)

class IsConnector p where
   getConnectorID :: p -> ConnectorID

instance IsConnector Connector where
   getConnectorID p = connectorID p

instance IsConnector ConnectorID where
   getConnectorID p = p


-- | Indicate if a cable is plugged in the connector
data Connection
   = Connected Display -- ^ A video display is connected
   | Disconnected           -- ^ No video display connected
   | ConnectionUnknown      -- ^ The connection state cannot be determined
   deriving (Show)

-- | Information about the connected video display
data Display = Display
   { displayModes          :: [Mode]        -- ^ Supported modes
   , displayPhysicalWidth  :: Word32        -- ^ Width (in millimeters)
   , displayPhysicalHeight :: Word32        -- ^ Height (in millimeters)
   , displaySubPixel       :: SubPixel      -- ^ Sub-pixel structure
   , displayProperties     :: [RawProperty] -- ^ Properties of the video display
   } deriving (Show)

-------------------------------------------------------------------------------
-- Encoder
-------------------------------------------------------------------------------

-- Note [Avoiding Encoders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Linux documentation recommends AGAINST using `encoderPossibleControllers` and
-- `encoderPossibleClones` because the drivers often report bad information.
-- Moreover it seems like the `Encoder` abstraction shouldn't have been exposed
-- to user-space because it isn't always meaningful. Moreover in the API the
-- encoder is chosen implicitly when we connect a connector to a controller.
--
-- In conclusion: we won't expose encoders in haskus-system to avoid all this
-- mess and because we don't really need them. The recommend Linux way to test
-- output cloning and other stuff is to use the Atomic API and to test the
-- different configurations until we find a suitable one.

-- | An encoder
--
-- An encoder converts data obtained from the controller (i.e. from the frame
-- buffer associated with the controller) into suitable data for the connector
-- (i.e. for the device connected to the connector). Hence it only supports a
-- set of connectors. In addition, it may not work with all controllers.
--
data Encoder = Encoder
   { encoderID                  :: EncoderID          -- ^ Encoder identifier
   , encoderType                :: EncoderType        -- ^ Type of the encoder
   , encoderControllerID        :: Maybe ControllerID -- ^ Associated controller
   , encoderPossibleControllers :: [ControllerID]     -- ^ Valid controllers
   , encoderPossibleClones      :: [EncoderID]        -- ^ Valid clone encoders
   , encoderHandle              :: Handle             -- ^ Graphic card
   } deriving (Show)

-------------------------------------------------------------------------------
-- Controller
-------------------------------------------------------------------------------

-- | Scanout controller
--
-- A controller is used to configure what is displayed on the screen
-- Controllers are called CRTC in original terminology
data Controller = Controller
   { controllerID             :: ControllerID  -- ^ Controller identifier
   , controllerMode           :: Maybe Mode
   , controllerFrame          :: Maybe FrameView   -- ^ Associated frame source and its position (x,y)
   , controllerGammaTableSize :: Word32
   , controllerHandle         :: Handle
   } deriving (Show)

data FrameView = FrameView
   { frameViewID :: FrameID        -- ^ Frame identifier
   , frameViewX  :: Word32         -- ^ Frame X position
   , frameViewY  :: Word32         -- ^ Frame Y position
   } deriving (Show)


class IsController p where
   getControllerID :: p -> ControllerID

instance IsController Controller where
   getControllerID p = controllerID p

instance IsController ControllerID where
   getControllerID p = p

-------------------------------------------------------------------------------
-- Plane
-------------------------------------------------------------------------------

-- | A plane
data Plane = Plane
   { planeID                  :: !PlaneID              -- ^ Plane identifier
   , planeControllerId        :: !(Maybe ControllerID) -- ^ Connected controller
   , planeFrameId             :: !(Maybe FrameID)      -- ^ Connected frame
   , planePossibleControllers :: ![ControllerID]       -- ^ Potential controllers
   , planeFormats             :: ![PixelFormat]        -- ^ Supported pixel formats
   , planeType                :: !PlaneType            -- ^ Plane type
   }
   deriving (Show)

class IsPlane p where
   getPlaneID :: p -> PlaneID

instance IsPlane Plane where
   getPlaneID p = planeID p

instance IsPlane PlaneID where
   getPlaneID p = p

type FP16_16 = FixedPoint Word32 16 16

data PlaneType
   = Primary
   | Overlay
   | Cursor
   deriving (Show,Eq,Ord)

-- | Plane target surface
--
-- (X,Y) coordinates can be negative for partial rendering (out-of-screen)
data PlaneTarget = PlaneTarget
   { planeTargetControllerID :: ControllerID
   , planeTargetX            :: Int32
   , planeTargetY            :: Int32
   , planeTargetWidth        :: Word32
   , planeTargetHeight       :: Word32
   }
   deriving (Show,Eq)

-- | Plane source pixel source
--
-- The fractional part in the fields is for devices supporting sub-pixel plane
-- coordinates.
data PlaneSource = PlaneSource
   { planeSourceFrameID :: FrameID
   , planeSourceX       :: FP16_16
   , planeSourceY       :: FP16_16
   , planeSourceWidth   :: FP16_16
   , planeSourceHeight  :: FP16_16
   }
   deriving (Show,Eq)


-------------------------------------------------------------------------------
-- Frame and FrameBuffer
-------------------------------------------------------------------------------

-- | Abstract frame source
data Frame b = Frame
   { frameID          :: FrameID         -- ^ Frame identifier
   , frameWidth       :: Word32          -- ^ Frame width
   , frameHeight      :: Word32          -- ^ Frame height
   , framePixelFormat :: PixelFormat     -- ^ Pixel format
   , frameFlags       :: FrameFlags      -- ^ Frame flags
   , frameBuffers     :: [FrameBuffer b] -- ^ Frame components buffers (up to four depending of pixel format)
   , frameCardHandle  :: Handle          -- ^ Card handle
   } deriving (Show)

class IsFrame p where
   getFrameID :: p -> FrameID

instance IsFrame (Frame a) where
   getFrameID p = frameID p

instance IsFrame FrameID where
   getFrameID p = p


-- | Frame buffer (contains components of the pixel colors)
data FrameBuffer b = FrameBuffer
   { fbBuffer       :: b      -- ^ Buffer
   , fbBufferHandle :: Word32 -- ^ Raw buffer handle
   , fbPitch        :: Word32 -- ^ Pitch of the frame in the buffer
   , fbOffset       :: Word32 -- ^ Offset of the frame in the buffer
   , fbModifiers    :: Word64 -- ^ Modifiers for the frame buffer (e.g. compression, tiling)
   } deriving (Show)

class ShowBuffer b where
   showBuffer :: b -> String

instance ShowBuffer () where
   showBuffer = const ""

-- | Show Frame fields
showFrame :: ShowBuffer b => Frame b -> String
showFrame Frame{..} = mconcat
   [ "Frame ", show (unEntityID frameID), "\n"
   , "  Width:  ", show frameWidth, " pixels\n"
   , "  Height: ", show frameHeight, " pixels\n"
   , "  Pixel format: ", show framePixelFormat, "\n"
   , "  Flags: ", show (BitSet.toList frameFlags), "\n"
   , mconcat (fmap myShowFB ([(0:: Word)..] `zip` frameBuffers))
   ]
   where
      myShowB b = showBuffer b
         |> lines
         ||> ("       - "<>)
         |> unlines
      myShowFB (n,fb) = mconcat
         [ "  FrameBuffer ", show n, ":\n"
         , showFrameBuffer fb |> lines ||> ("    - "<>) |> unlines
         , "    - Buffer specific details:\n", myShowB (fbBuffer fb)
         ]

-- | Show FrameBuffer
showFrameBuffer :: FrameBuffer b -> String
showFrameBuffer FrameBuffer{..} = mconcat
   [ "Buffer handle: ", show fbBufferHandle
   , "\nPitch: ", show fbPitch
   , "\nOffset: ", show fbOffset
   , "\nModifiers: ", show fbModifiers
   , "\n"
   ]

-------------------------------------------------------------------------------
-- Property
-------------------------------------------------------------------------------


-- | Property meta-information
data PropertyMeta = PropertyMeta
   { propertyID        :: PropertyID   -- ^ ID of the property
   , propertyImmutable :: Bool         -- ^ The value won't change
   , propertyName      :: String       -- ^ Property name
   , propertyType      :: PropertyType -- ^ Type of the property
   } deriving (Show,Eq)

-- | The type of a property
data PropertyType
   = PropRange       [Word64]          -- ^ A range
   | PropSignedRange [Int64]           -- ^ A signed range
   | PropEnum        [(Word64,String)] -- ^ Value-enum
   | PropBitmask     [(Word64,String)] -- ^ Bit-enum (bitmask)
   | PropBlob        [(Word32,Buffer)] -- ^ Blob-enum
   | PropObject
   deriving (Show,Eq)

data RawProperty = RawProperty
   { rawPropertyID    :: PropertyID -- ^ Card-wise property meta-info ID
   , rawPropertyValue :: Word64     -- ^ Property value
   } deriving (Show,Eq)

data Property = Property
   { propertyMeta       :: PropertyMeta -- ^ Meta-information about the property
   , propertyValue      :: Word64       -- ^ Value of the property
   } deriving (Show,Eq)

