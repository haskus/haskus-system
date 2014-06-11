-- | OpenCL imaging API
module ViperVM.Arch.OpenCL.ImageFormat (
   ImageFormat(..), AddressingMode(..),
   FilterMode(..), ChannelOrder(..), ChannelType(..)
) where

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CDouble)
import Data.Word (Word32)

import ViperVM.Arch.OpenCL.Bindings (CLConstant(..))

-- | Image storage format
data ImageFormat = ImageFormat {
   imageChannelOrder :: !ChannelOrder,
   imageChannelType :: !ChannelType
} deriving (Show)

instance Storable ImageFormat where
   alignment _ = alignment (undefined :: CDouble)
   sizeOf _ = 64
   peek p = do
      a <- fmap fromCL (peekByteOff p 0 :: IO Word32)
      b <- fmap fromCL (peekByteOff p 4 :: IO Word32)
      return $ ImageFormat a b
   poke p (ImageFormat a b) = do
      pokeByteOff p 0 (toCL a :: Word32)
      pokeByteOff p 4 (toCL b :: Word32)

-- | Image addressing mode
data AddressingMode =
     CL_ADDRESS_NONE
   | CL_ADDRESS_CLAMP_TO_EDGE
   | CL_ADDRESS_CLAMP
   | CL_ADDRESS_REPEAT
   | CL_ADDRESS_MIRRORED_REPEAT
   deriving (Show,Enum)

instance CLConstant AddressingMode where
   toCL x = fromIntegral (fromEnum x + 0x1130)
   fromCL x = toEnum (fromIntegral x - 0x1130)

-- | Image fitlering mode
data FilterMode =
     CL_FILTER_NEAREST
   | CL_FILTER_LINEAR
   deriving (Show,Enum)

instance CLConstant FilterMode where
   toCL x = fromIntegral (fromEnum x + 0x1140)
   fromCL x = toEnum (fromIntegral x - 0x1140)

-- | Image channel order
data ChannelOrder =
     CL_R                         
   | CL_A                         
   | CL_RG                        
   | CL_RA                        
   | CL_RGB                       
   | CL_RGBA                      
   | CL_BGRA                      
   | CL_ARGB                      
   | CL_INTENSITY                 
   | CL_LUMINANCE                 
   | CL_Rx                        
   | CL_RGx                       
   | CL_RGBx                      
   | CL_DEPTH                     
   | CL_DEPTH_STENCIL             
   deriving (Show,Enum)

instance CLConstant ChannelOrder where
   toCL x = fromIntegral (0x10B0 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x10B0)

-- | Image channel type
data ChannelType = 
     CL_SNORM_INT8                 
   | CL_SNORM_INT16                
   | CL_UNORM_INT8                 
   | CL_UNORM_INT16                
   | CL_UNORM_SHORT_565            
   | CL_UNORM_SHORT_555            
   | CL_UNORM_INT_101010           
   | CL_SIGNED_INT8                
   | CL_SIGNED_INT16               
   | CL_SIGNED_INT32               
   | CL_UNSIGNED_INT8              
   | CL_UNSIGNED_INT16             
   | CL_UNSIGNED_INT32             
   | CL_HALF_FLOAT                 
   | CL_FLOAT                      
   | CL_UNORM_INT24                
   deriving (Show,Enum)

instance CLConstant ChannelType where
   toCL x = fromIntegral (0x10D0 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x10D0)
