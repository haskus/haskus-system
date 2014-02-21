module ViperVM.Platform.OpenCL.ImageFormat where

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CDouble)
import Data.Word (Word32)

import ViperVM.Platform.OpenCL.Channel
import ViperVM.Platform.OpenCL.Bindings (CLConstant(..))

data CLImageFormat = CLImageFormat {
   image_channel_order :: !CLChannelOrder,
   image_channel_data_type :: !CLChannelType
} deriving (Show)

instance Storable CLImageFormat where
   alignment _ = alignment (undefined :: CDouble)
   sizeOf _ = 64
   peek p = do
      a <- fmap fromCL (peekByteOff p 0 :: IO Word32)
      b <- fmap fromCL (peekByteOff p 4 :: IO Word32)
      return $ CLImageFormat a b
   poke p (CLImageFormat a b) = do
      pokeByteOff p 0 (toCL a :: Word32)
      pokeByteOff p 4 (toCL b :: Word32)

