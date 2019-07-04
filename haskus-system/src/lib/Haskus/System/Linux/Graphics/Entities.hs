{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.System.Linux.Graphics.Entities
   ( -- * IDs
     EntityID (..)
   , FrameID
   , ControllerID
   , ConnectorID
   , EncoderID
   , PlaneID
   , BlobID
   -- * Connector
   , Connector (..)
   , Connection (..)
   , Display (..)
   -- * Encoder
   , Encoder (..)
   , EncoderType (..)
   -- * Controller
   , Controller (..)
   , FrameView (..)
   -- * Plane
   , Plane (..)
   , PlaneTarget (..)
   , PlaneSource (..)
   -- * Frame
   , Frame (..)
   , FrameBuffer (..)
   , showFrame
   , showFrameBuffer
   , ShowBuffer(..)
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.FixedPoint
import qualified Haskus.Format.Binary.BitSet as BitSet
import Haskus.System.Linux.Internals.Graphics
import Haskus.System.Linux.Handle
import Haskus.System.Linux.Graphics.Mode
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.PixelFormat
import Haskus.Utils.Flow

-------------------------------------------------------------------------------
-- IDs
-------------------------------------------------------------------------------

-- | Entity identifier
newtype EntityID a = EntityID
   { unEntityID :: Word32
   } deriving (Show,Eq,Storable,Ord)

type FrameID       = EntityID (Frame ())
type ConnectorID   = EntityID Connector
type ControllerID  = EntityID Controller
type EncoderID     = EntityID Encoder
type PlaneID       = EntityID Plane
type BlobID x      = EntityID x

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
