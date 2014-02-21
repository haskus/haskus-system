module ViperVM.Platform.OpenCL.Channel where

import ViperVM.Platform.OpenCL.Bindings (CLConstant(..))

data CLChannelOrder =
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

instance CLConstant CLChannelOrder where
   toCL x = fromIntegral (0x10B0 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x10B0)

data CLChannelType = 
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

instance CLConstant CLChannelType where
   toCL x = fromIntegral (0x10D0 + fromEnum x)
   fromCL x = toEnum (fromIntegral x - 0x10D0)


