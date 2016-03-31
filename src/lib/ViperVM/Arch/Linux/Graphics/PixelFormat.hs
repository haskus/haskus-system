{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pixel formats
module ViperVM.Arch.Linux.Graphics.PixelFormat
   ( PixelFormat (..)
   , makePixelFormat
   , Format(..)
   , Endianness(..)
   , formatEndianness
   , formatFormat
   , formatBitDepth
   )
where

import ViperVM.Format.Binary.Endianness
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.BitField
import Data.Word
import Data.Char (ord)
import Data.Proxy
import Data.Map (Map, (!), fromList)
import Data.Tuple (swap)
import Foreign.Storable
import Foreign.CStorable

-- =============================================================
--    From linux/include/uapi/drm/drm_fourcc.h
-- =============================================================

-- | A pixel format is represented as a Word32
newtype PixelFormat = PixelFormat (BitFields Word32
  '[ BitField 1  "endianness" (EnumField Int Endianness)
   , BitField 31 "format"     (EnumField Word32 Format)
   ]) deriving (Storable)

instance Show PixelFormat where
   show fmt = show (formatFormat fmt) ++ " ("
               ++ show (formatEndianness fmt) ++ ")"

makePixelFormat :: Format -> Endianness -> PixelFormat
makePixelFormat fmt end = PixelFormat
   $ updateField (Proxy :: Proxy "endianness") (toEnumField end)
   $ updateField (Proxy :: Proxy "format")     (toEnumField fmt)
   $ BitFields 0

{-# INLINE makePixelFormat #-}

-- | Get pixel format endianness
formatEndianness :: PixelFormat -> Endianness
formatEndianness (PixelFormat fmt) =
   fromEnumField (extractField (Proxy :: Proxy "endianness") fmt)
{-# INLINE formatEndianness #-}

-- | Get pixel format logical format
formatFormat :: PixelFormat -> Format
formatFormat (PixelFormat fmt) = 
   fromEnumField (extractField (Proxy :: Proxy "format") fmt)
{-# INLINE formatFormat #-}

instance CStorable PixelFormat where
   cSizeOf      = sizeOf
   cAlignment   = alignment
   cPeek        = peek
   cPoke        = poke

type CFormat = BitFields Word32
  '[ BitField 8 "d" Int
   , BitField 8 "c" Int
   , BitField 8 "b" Int
   , BitField 8 "a" Int
   ]

-- | Convert a format string (as in the original .h file) into a code
toFormat :: String -> Word32
toFormat [a,b,c,d] = bitFieldsBits
   $ updateField (Proxy :: Proxy "a") (ord a)
   $ updateField (Proxy :: Proxy "b") (ord b)
   $ updateField (Proxy :: Proxy "c") (ord c)
   $ updateField (Proxy :: Proxy "d") (ord d)
   $ (BitFields 0 :: CFormat)
toFormat _ = undefined
{-# INLINE toFormat #-}


fmt2bits :: Map Format Word32
fmt2bits = fromList codes

bits2fmt :: Map Word32 Format
bits2fmt = fromList (map swap codes)

-- | Associate formats to their fourcc code
codes :: [(Format,Word32)]
codes =
   [(C8          , toFormat "C8  ")
   ,(RGB332      , toFormat "RGB8")
   ,(BGR233      , toFormat "BGR8")
   
   ,(XRGB4444    , toFormat "XR12")
   ,(XBGR4444    , toFormat "XB12")
   ,(RGBX4444    , toFormat "RX12")
   ,(BGRX4444    , toFormat "BX12")
   
   ,(ARGB4444    , toFormat "AR12")
   ,(ABGR4444    , toFormat "AB12")
   ,(RGBA4444    , toFormat "RA12")
   ,(BGRA4444    , toFormat "BA12")
   
   ,(XRGB1555    , toFormat "XR15")
   ,(XBGR1555    , toFormat "XB15")
   ,(RGBX5551    , toFormat "RX15")
   ,(BGRX5551    , toFormat "BX15")
   
   ,(ARGB1555    , toFormat "AR15")
   ,(ABGR1555    , toFormat "AB15")
   ,(RGBA5551    , toFormat "RA15")
   ,(BGRA5551    , toFormat "BA15")
   
   ,(RGB565      , toFormat "RG16")
   ,(BGR565      , toFormat "BG16")
   
   ,(RGB888      , toFormat "RG24")
   ,(BGR888      , toFormat "BG24")
   
   ,(XRGB8888    , toFormat "XR24")
   ,(XBGR8888    , toFormat "XB24")
   ,(RGBX8888    , toFormat "RX24")
   ,(BGRX8888    , toFormat "BX24")
   
   ,(ARGB8888    , toFormat "AR24")
   ,(ABGR8888    , toFormat "AB24")
   ,(RGBA8888    , toFormat "RA24")
   ,(BGRA8888    , toFormat "BA24")
   
   ,(XRGB2101010 , toFormat "XR30")
   ,(XBGR2101010 , toFormat "XB30")
   ,(RGBX1010102 , toFormat "RX30")
   ,(BGRX1010102 , toFormat "BX30")
   
   ,(ARGB2101010 , toFormat "AR30")
   ,(ABGR2101010 , toFormat "AB30")
   ,(RGBA1010102 , toFormat "RA30")
   ,(BGRA1010102 , toFormat "BA30")
   
   ,(YUYV        , toFormat "YUYV")
   ,(YVYU        , toFormat "YVYU")
   ,(UYVY        , toFormat "UYVY")
   ,(VYUY        , toFormat "VYUY")
   
   ,(AYUY        , toFormat "AYUY")
   
   ,(NV12        , toFormat "NV12")
   ,(NV21        , toFormat "NV21")
   ,(NV16        , toFormat "NV16")
   ,(NV61        , toFormat "NV61")
   
   ,(YUV410      , toFormat "YUV9")
   ,(YVU410      , toFormat "YVU9")
   ,(YUV411      , toFormat "YU11")
   ,(YVU411      , toFormat "YV11")
   ,(YUV420      , toFormat "YU12")
   ,(YVU420      , toFormat "YV12")
   ,(YUV422      , toFormat "YU16")
   ,(YVU422      , toFormat "YV16")
   ,(YUV444      , toFormat "YU24")
   ,(YVU444      , toFormat "YV24")
   ]

-- | Bit-depth per plane
formatBitDepth :: Format -> [Word32]
formatBitDepth fmt = case fmt of
   C8           -> [8]
   RGB332       -> [8]
   BGR233       -> [8]
   XRGB4444     -> [16]
   XBGR4444     -> [16]
   RGBX4444     -> [16]
   BGRX4444     -> [16]
   ARGB4444     -> [16]
   ABGR4444     -> [16]
   RGBA4444     -> [16]
   BGRA4444     -> [16]
   XRGB1555     -> [16]
   XBGR1555     -> [16]
   RGBX5551     -> [16]
   BGRX5551     -> [16]
   ARGB1555     -> [16]
   ABGR1555     -> [16]
   RGBA5551     -> [16]
   BGRA5551     -> [16]
   RGB565       -> [16]
   BGR565       -> [16]
   RGB888       -> [24]
   BGR888       -> [24]
   XRGB8888     -> [32]
   XBGR8888     -> [32]
   RGBX8888     -> [32]
   BGRX8888     -> [32]
   ARGB8888     -> [32]
   ABGR8888     -> [32]
   RGBA8888     -> [32]
   BGRA8888     -> [32]
   XRGB2101010  -> [32]
   XBGR2101010  -> [32]
   RGBX1010102  -> [32]
   BGRX1010102  -> [32]
   ARGB2101010  -> [32]
   ABGR2101010  -> [32]
   RGBA1010102  -> [32]
   BGRA1010102  -> [32]
   YUYV         -> [32]
   YVYU         -> [32]
   UYVY         -> [32]
   VYUY         -> [32]
   AYUY         -> [32]
   NV12         -> [8,16]
   NV21         -> [8,16]
   NV16         -> [8,16]
   NV61         -> [8,16]
   YUV410       -> [8,8,8]
   YVU410       -> [8,8,8]
   YUV411       -> [8,8,8]
   YVU411       -> [8,8,8]
   YUV420       -> [8,8,8]
   YVU420       -> [8,8,8]
   YUV422       -> [8,8,8]
   YVU422       -> [8,8,8]
   YUV444       -> [8,8,8]
   YVU444       -> [8,8,8]


-- | Logical pixel format (i.e. without considering storage endianness)
data Format
   = C8                 -- ^ 8 bits, color index

   | RGB332             -- ^ 8 bpp RGB: R:G:B 3:3:2
   | BGR233             -- ^ 8 bpp RGB: B:G:R 2:3:3

   | XRGB4444           -- ^ 16 bpp RGB: x:R:G:B 4:4:4:4
   | XBGR4444           -- ^ 16 bpp RGB: x:B:G:R 4:4:4:4
   | RGBX4444           -- ^ 16 bpp RGB: R:G:B:x 4:4:4:4
   | BGRX4444           -- ^ 16 bpp RGB: B:G:R:x 4:4:4:4
   | ARGB4444           -- ^ 16 bpp ARGB: A:R:G:B 4:4:4:4

   | ABGR4444           -- ^ 16 bpp ARGB: A:B:G:R 4:4:4:4
   | RGBA4444           -- ^ 16 bpp ARGB: R:G:B:A 4:4:4:4
   | BGRA4444           -- ^ 16 bpp ARGB: B:G:R:A 4:4:4:4

   | XRGB1555           -- ^ 16 bpp RGB: x:R:G:B 1:5:5:5
   | XBGR1555           -- ^ 16 bpp RGB: x:B:G:R 1:5:5:5
   | RGBX5551           -- ^ 16 bpp RGB: R:G:B:x 5:5:5:1
   | BGRX5551           -- ^ 16 bpp RGB: B:G:R:x 5:5:5:1

   | ARGB1555           -- ^ 16 bpp ARGB: A:R:G:B 1:5:5:5
   | ABGR1555           -- ^ 16 bpp ARGB: A:B:G:R 1:5:5:5
   | RGBA5551           -- ^ 16 bpp ARGB: R:G:B:A 5:5:5:1
   | BGRA5551           -- ^ 16 bpp ARGB: B:G:R:A 5:5:5:1

   | RGB565             -- ^ 16 bpp RGB:  R:G:B 5:6:5
   | BGR565             -- ^ 16 bpp RGB:  B:G:R 5:6:5

   | RGB888             -- ^ 24 bpp RGB:  R:G:B 8:8:8
   | BGR888             -- ^ 24 bpp RGB:  B:G:R 8:8:8

   | XRGB8888           -- ^ 32 bpp RGB:  x:R:G:B 8:8:8:8
   | XBGR8888           -- ^ 32 bpp RGB:  x:B:G:R 8:8:8:8
   | RGBX8888           -- ^ 32 bpp RGB:  R:G:B:x 8:8:8:8
   | BGRX8888           -- ^ 32 bpp RGB:  B:G:R:x 8:8:8:8

   | ARGB8888           -- ^ 32 bpp ARGB: A:R:G:B 8:8:8:8
   | ABGR8888           -- ^ 32 bpp ARGB: A:B:G:R 8:8:8:8
   | RGBA8888           -- ^ 32 bpp ARGB: R:G:B:A 8:8:8:8
   | BGRA8888           -- ^ 32 bpp ARGB: B:G:R:A 8:8:8:8

   | XRGB2101010        -- ^ 32 bpp RGB:  x:R:G:B 2:10:10:10
   | XBGR2101010        -- ^ 32 bpp RGB:  x:B:G:R 2:10:10:10
   | RGBX1010102        -- ^ 32 bpp RGB:  R:G:B:x 10:10:10:2
   | BGRX1010102        -- ^ 32 bpp RGB:  B:G:R:x 10:10:10:2

   | ARGB2101010        -- ^ 32 bpp ARGB: A:R:G:B 2:10:10:10
   | ABGR2101010        -- ^ 32 bpp ARGB: A:B:G:R 2:10:10:10
   | RGBA1010102        -- ^ 32 bpp ARGB: R:G:B:A 10:10:10:2
   | BGRA1010102        -- ^ 32 bpp ARGB: B:G:R:A 10:10:10:2

   | YUYV               -- ^ Packed YCbCr: Cr0:Y1:Cb0:Y0 8:8:8:8
   | YVYU               -- ^ Packed YCbCr: Cb0:Y1:Cr0:Y0 8:8:8:8
   | UYVY               -- ^ Packed YCbCr: Y1:Cr0:Y0:Cb0 8:8:8:8
   | VYUY               -- ^ Packed YCbCr: Y1:Cb0:Y0:Cr0 8:8:8:8

   | AYUY               -- ^ Packed YCbCr: A:Y:Cb:Cr 8:8:8:8

   -- 2 plane YCbCr
   -- index 0 = Y plane, [7:0] Y
   -- index 1 = Cr:Cb plane, [15:0] Cr:Cb
   -- or
   -- index 1 = Cb:Cr plane, [15:0] Cb:Cr
   | NV12               -- ^ 2 plane YCbCr: 2x2 subsampled Cr:Cb plane
   | NV21               -- ^ 2 plane YCbCr: 2x2 subsampled Cb:Cr plane
   | NV16               -- ^ 2 plane YCbCr: 2x1 subsampled Cr:Cb plane
   | NV61               -- ^ 2 plane YCbCr: 2x1 subsampled Cb:Cr plane

   -- 3 plane YCbCr
   -- index 0: Y plane, [7:0] Y
   -- index 1: Cb plane, [7:0] Cb
   -- index 2: Cr plane, [7:0] Cr
   -- or
   -- index 1: Cr plane, [7:0] Cr
   -- index 2: Cb plane, [7:0] Cb
   | YUV410             -- ^ 3 plane YCbCr: 4x4 subsampled Cb (1) and Cr (2) planes
   | YVU410             -- ^ 3 plane YCbCr: 4x4 subsampled Cr (1) and Cb (2) planes
   | YUV411             -- ^ 3 plane YCbCr: 4x1 subsampled Cb (1) and Cr (2) planes
   | YVU411             -- ^ 3 plane YCbCr: 4x1 subsampled Cr (1) and Cb (2) planes
   | YUV420             -- ^ 3 plane YCbCr: 2x2 subsampled Cb (1) and Cr (2) planes
   | YVU420             -- ^ 3 plane YCbCr: 2x2 subsampled Cr (1) and Cb (2) planes
   | YUV422             -- ^ 3 plane YCbCr: 2x1 subsampled Cb (1) and Cr (2) planes
   | YVU422             -- ^ 3 plane YCbCr: 2x1 subsampled Cr (1) and Cb (2) planes
   | YUV444             -- ^ 3 plane YCbCr: non-subsampled Cb (1) and Cr (2) planes
   | YVU444             -- ^ 3 plane YCbCr: non-subsampled Cr (1) and Cb (2) planes
   deriving (Eq,Ord,Show,Enum)

instance CEnum Format where
   fromCEnum x = fromIntegral (fmt2bits ! x)
   toCEnum   x = bits2fmt ! (fromIntegral x)
      
