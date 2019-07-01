{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskus.System.Linux.Graphics.Entities
   ( -- * IDs
     EntityID (..)
   , FrameID
   , ControllerID
   , ConnectorID
   , EncoderID
   , PlaneID
   -- * Connector
   , Connector (..)
   , Connection (..)
   , VideoDisplay (..)
   -- * Encoder
   , Encoder (..)
   , EncoderType (..)
   -- * Controller
   , Controller (..)
   , FrameView (..)
   -- * Plane
   , Plane (..)
   , DestRect (..)
   , SrcRect (..)
   -- * Frame source
   , Frame (..)
   , FrameBuffer (..)
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.FixedPoint
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.PixelFormat

-------------------------------------------------------------------------------
-- IDs
-------------------------------------------------------------------------------

-- | Entity identifier
newtype EntityID a = EntityID
   { unEntityID :: Word32
   } deriving (Show,Eq,Storable,Ord)

type FrameID = EntityID (Frame ())
type ConnectorID   = EntityID Connector
type ControllerID  = EntityID Controller
type EncoderID     = EntityID Encoder
type PlaneID       = EntityID Plane

-------------------------------------------------------------------------------
-- Connector
-------------------------------------------------------------------------------

-- | A connector on the graphic card
data Connector = Connector
   { connectorID                 :: ConnectorID     -- ^ Connector identifier
   , connectorType               :: ConnectorType   -- ^ Type of connector
   , connectorByTypeIndex        :: Word32          -- ^ Identifier within connectors of the same type
   , connectorState              :: Connection      -- ^ Connection state
   , connectorPossibleEncoderIDs :: [EncoderID]     -- ^ IDs of the encoders that can work with this connector
   , connectorEncoderID          :: Maybe EncoderID -- ^ Currently used encoder
   , connectorHandle             :: Handle          -- ^ Graphic card
   } deriving (Show)

-- | Indicate if a cable is plugged in the connector
data Connection
   = Connected VideoDisplay -- ^ A video display is connected
   | Disconnected           -- ^ No video display connected
   | ConnectionUnknown      -- ^ The connection state cannot be determined
   deriving (Show)

-- | Information about the connected video display
data VideoDisplay = VideoDisplay
   { videoModes          :: [Mode]     -- ^ Supported modes
   , videoPhysicalWidth  :: Word32     -- ^ Width (in millimeters)
   , videoPhysicalHeight :: Word32     -- ^ Height (in millimeters)
   , videoSubPixel       :: SubPixel   -- ^ Sub-pixel structure
   , videoProperties     :: [Property] -- ^ Properties of the video display
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

-------------------------------------------------------------------------------
-- Plane
-------------------------------------------------------------------------------

-- | A plane
data Plane = Plane
   { planeID                  :: PlaneID              -- ^ Plane identifier
   , planeControllerId        :: Maybe ControllerID   -- ^ Connected controller
   , planeFrameId             :: Maybe FrameID        -- ^ Connected frame
   , planePossibleControllers :: [ControllerID]       -- ^ Potential controllers
   , planeGammaSize           :: Word32               -- ^ Size of the gamma table
   , planeFormats             :: [PixelFormat]        -- ^ Supported pixel formats
   }
   deriving (Show)

type FP16_16 = FixedPoint Word32 16 16

-- | Destination rectangle
data DestRect = DestRect
   { destX      :: Int32
   , destY      :: Int32
   , destWidth  :: Word32
   , destHeight :: Word32
   }
   deriving (Show,Eq)

-- | Source rectangle
data SrcRect = SrcRect
   { srcX      :: FP16_16
   , srcY      :: FP16_16
   , srcWidth  :: FP16_16
   , srcHeight :: FP16_16
   }
   deriving (Show,Eq)


-------------------------------------------------------------------------------
-- Frame source
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

-- | Frame buffer (contains components of the pixel colors)
data FrameBuffer b = FrameBuffer
   { fbBuffer       :: b      -- ^ Buffer
   , fbBufferHandle :: Word32 -- ^ Raw buffer handle
   , fbPitch        :: Word32 -- ^ Pitch of the frame in the buffer
   , fbOffset       :: Word32 -- ^ Offset of the frame in the buffer
   , fbModifiers    :: Word64 -- ^ Modifiers for the frame buffer
   } deriving (Show)

