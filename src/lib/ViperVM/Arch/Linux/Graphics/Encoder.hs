{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Encoders management
--
-- An encoder converts data obtained from the controller (i.e. from the frame
-- buffer associated with the controller) into suitable data for the connector
-- (i.e. for the device connected to the connector). Hence it only supports a
-- set of connectors. In addition, it may not work with all controllers.
module ViperVM.Arch.Linux.Graphics.Encoder
   ( Encoder(..)
   , EncoderType(..)
   , encoderController
   , getEncoders
   , getEncoderFromID
   )
where

import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Binary.Enum

-- | An encoder
data Encoder = Encoder
   { encoderID                  :: EncoderID          -- ^ Encoder identifier
   , encoderType                :: EncoderType        -- ^ Type of the encoder
   , encoderControllerID        :: Maybe ControllerID -- ^ Associated controller
   , encoderPossibleControllers :: [ControllerID]     -- ^ Valid controllers
   , encoderPossibleClones      :: [EncoderID]        -- ^ Valid clone encoders
   , encoderHandle              :: Handle             -- ^ Graphic card
   } deriving (Show)

fromStructGetEncoder :: Resources -> Handle -> StructGetEncoder -> Encoder
fromStructGetEncoder res hdl StructGetEncoder{..} =
      Encoder
         (EncoderID geEncoderId)
         (fromEnumField geEncoderType)
         (if geCrtcId == 0
            then Nothing
            else Just (ControllerID geCrtcId))
         (pick' (resControllerIDs res) gePossibleCrtcs)
         (pick' (resEncoderIDs    res) gePossibleClones)
         hdl
   where
      -- pick the elements in es whose indexes are in bs
      pick' es bs = pick es 0 (BitSet.elems bs)

      pick :: [a] -> Int -> [Int] -> [a]
      pick [] _ _ = []
      pick _ _ [] = []
      pick (x:xs) n (i:is)
         | n == i    = x : pick xs (n+1) is
         | otherwise = pick xs (n+1) (i:is)

-- | Get an encoder from its ID
getEncoderFromID :: Handle -> EncoderID -> SysRet Encoder
getEncoderFromID hdl (EncoderID encId) = do
   let enc = StructGetEncoder encId (toEnumField EncoderTypeNone)
               0 BitSet.empty BitSet.empty
   res <- runSys $ sysCallAssert "Get resources" $ getResources hdl
   fmap (fromStructGetEncoder res hdl) <$> ioctlGetEncoder hdl enc

-- | Controller attached to the encoder, if any
encoderController :: Encoder -> SysRet (Maybe Controller)
encoderController enc = case encoderControllerID enc of
   Nothing     -> return (Right Nothing)
   Just contId -> fmap Just <$> getControllerFromID (encoderHandle enc) contId

-- | Get encoders (discard errors)
getEncoders :: Handle -> IO [Encoder]
getEncoders = getEntities resEncoderIDs getEncoderFromID
