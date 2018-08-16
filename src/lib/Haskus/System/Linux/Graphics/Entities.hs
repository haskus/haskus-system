{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskus.System.Linux.Graphics.Entities
   ( -- * IDs
     EntityID (..)
   , FrameSourceID
   , ControllerID
   , ConnectorID
   , EncoderID
   , PlaneID
   -- * Connector
   , Connector (..)
   , Connection (..)
   , ConnectedDevice (..)
   -- * Encoder
   , Encoder (..)
   , EncoderType (..)
   -- * Controller
   , Controller (..)
   , Frame (..)
   -- * Plane
   , Plane (..)
   -- * Frame source
   , FrameSource (..)
   , PixelSource (..)
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
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

type FrameSourceID = EntityID FrameSource
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
   = Connected ConnectedDevice -- ^ The connector is connected to a displaying device
   | Disconnected              -- ^ The connector is disconnected
   | ConnectionUnknown         -- ^ The connection state cannot be determined
   deriving (Show)

-- | Information about the connected device
data ConnectedDevice = ConnectedDevice
   { connectedDeviceModes        :: [Mode]     -- ^ Supported modes
   , connectedDeviceWidth        :: Word32     -- ^ Width (in millimeters)
   , connectedDeviceHeight       :: Word32     -- ^ Height (in millimeters)
   , connectedDeviceSubPixel     :: SubPixel   -- ^ Sub-pixel structure
   , connectedDeviceProperties   :: [Property] -- ^ Properties of the connector
   } deriving (Show)

-------------------------------------------------------------------------------
-- Encoder
-------------------------------------------------------------------------------

-- | An encoder
--
-- An encoder converts data obtained from the controller (i.e. from the frame
-- buffer associated with the controller) into suitable data for the connector
-- (i.e. for the device connected to the connector). Hence it only supports a
-- set of connectors. In addition, it may not work with all controllers.
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
   { controllerID             :: ControllerID         -- ^ Controller identifier
   , controllerMode           :: Maybe Mode
   , controllerFrameBuffer    :: Maybe Frame -- ^ Associated frame buffer and its position (x,y)
   , controllerGammaTableSize :: Word32
   , controllerHandle         :: Handle
   } deriving (Show)

data Frame = Frame
   { frameBufferPosID :: FrameSourceID        -- ^ Framebuffer identifier
   , frameBufferPosX  :: Word32
   , frameBufferPosY  :: Word32
   } deriving (Show)

-------------------------------------------------------------------------------
-- Plane
-------------------------------------------------------------------------------

-- | A plane
data Plane = Plane
   { planeID                  :: PlaneID              -- ^ Plane identifier
   , planeControllerId        :: Maybe ControllerID   -- ^ Connected controller
   , planeFrameBufferId       :: Maybe FrameSourceID  -- ^ Connected framebuffer
   , planePossibleControllers :: [ControllerID]       -- ^ Potential controllers
   , planeGammaSize           :: Word32               -- ^ Size of the gamma table
   , planeFormats             :: [PixelFormat]        -- ^ Supported pixel formats
   }
   deriving (Show)

-------------------------------------------------------------------------------
-- Frame source
-------------------------------------------------------------------------------

-- | Abstract frame source
data FrameSource = FrameSource
   { frameID          :: FrameSourceID    -- ^ Frame buffer identifier
   , frameWidth       :: Word32           -- ^ Frame buffer width
   , frameHeight      :: Word32           -- ^ Frame buffer height
   , framePixelFormat :: PixelFormat      -- ^ Pixel format
   , frameFlags       :: FrameBufferFlags -- ^ Flags
   , frameSources     :: [PixelSource]    -- ^ Data sources (up to four)
   } deriving (Show)

-- | Pixel source
data PixelSource = PixelSource
   { surfaceHandle    :: Word32 -- ^ Handle of the surface
   , surfacePitch     :: Word32 -- ^ Pitch of the surface
   , surfaceOffset    :: Word32 -- ^ Offset of the surface
   , surfaceModifiers :: Word64 -- ^ Modifiers for the surface
   } deriving (Show)

