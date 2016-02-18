-- | Pixel formats
--
-- Taken from drm/drm_fourcc.h
module ViperVM.Arch.Linux.Graphics.PixelFormat
   ( PixelFormat(..)
   , Format(..)
   , Endianness(..)
   , formatBitDepth
   )
where

import ViperVM.Format.Binary.Endianness
import Data.Word
import Data.Char (ord)
import Data.Bits
import Data.Map (Map, (!), fromList)
import Data.Tuple (swap)
import Control.Arrow (second)
import Foreign.Storable
import Foreign.Ptr

-- | Pixel format
data PixelFormat
   = PixelFormat Format Endianness
   deriving (Eq,Show)

instance Storable PixelFormat where
   sizeOf _    = 4
   alignment _ = 4
   peek ptr    = do
      w <- peek (castPtr ptr :: Ptr Word32)
      let 
         endian = case testBit w 31 of
            True  -> BigEndian
            False -> LittleEndian
         fmt = bits2fmt ! (clearBit w 31)

      return $ PixelFormat fmt endian

   poke ptr (PixelFormat fmt endian) = do
      let
         fmt' = fmt2bits ! fmt
         w = case endian of
            BigEndian    -> fmt' `setBit` 31
            LittleEndian -> fmt'
      poke (castPtr ptr :: Ptr Word32) w
   

fmt2bits :: Map Format Word32
fmt2bits = fromList assoc

bits2fmt :: Map Word32 Format
bits2fmt = fromList (map swap assoc)

-- | Associate formats to their fourcc code
assoc :: [(Format,Word32)]
assoc = assoc'
   where
      f (a,b,c,d) = a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)
      k = fromIntegral . ord
      g [a,b,c,d] = f (k a, k b, k c, k d)
      g _ = undefined
      assoc' = map (second g)
         [(C8         , "C8  ")
         ,(RGB332     , "RGB8")
         ,(BGR233     , "BGR8")

         ,(XRGB4444   , "XR12")
         ,(XBGR4444   , "XB12")
         ,(RGBX4444   , "RX12")
         ,(BGRX4444   , "BX12")

         ,(ARGB4444   , "AR12")
         ,(ABGR4444   , "AB12")
         ,(RGBA4444   , "RA12")
         ,(BGRA4444   , "BA12")

         ,(XRGB1555   , "XR15")
         ,(XBGR1555   , "XB15")
         ,(RGBX5551   , "RX15")
         ,(BGRX5551   , "BX15")

         ,(ARGB1555   , "AR15")
         ,(ABGR1555   , "AB15")
         ,(RGBA5551   , "RA15")
         ,(BGRA5551   , "BA15")

         ,(RGB565     , "RG16")
         ,(BGR565     , "BG16")

         ,(RGB888     , "RG24")
         ,(BGR888     , "BG24")

         ,(XRGB8888   , "XR24")
         ,(XBGR8888   , "XB24")
         ,(RGBX8888   , "RX24")
         ,(BGRX8888   , "BX24")

         ,(ARGB8888   , "AR24")
         ,(ABGR8888   , "AB24")
         ,(RGBA8888   , "RA24")
         ,(BGRA8888   , "BA24")

         ,(XRGB2101010 , "XR30")
         ,(XBGR2101010 , "XB30")
         ,(RGBX1010102 , "RX30")
         ,(BGRX1010102 , "BX30")

         ,(ARGB2101010 , "AR30")
         ,(ABGR2101010 , "AB30")
         ,(RGBA1010102 , "RA30")
         ,(BGRA1010102 , "BA30")

         ,(YUYV        , "YUYV")
         ,(YVYU        , "YVYU")
         ,(UYVY        , "UYVY")
         ,(VYUY        , "VYUY")

         ,(AYUY        , "AYUY")

         ,(NV12        , "NV12")
         ,(NV21        , "NV21")
         ,(NV16        , "NV16")
         ,(NV61        , "NV61")

         ,(YUV410      , "YUV9")
         ,(YVU410      , "YVU9")
         ,(YUV411      , "YU11")
         ,(YVU411      , "YV11")
         ,(YUV420      , "YU12")
         ,(YVU420      , "YV12")
         ,(YUV422      , "YU16")
         ,(YVU422      , "YV16")
         ,(YUV444      , "YU24")
         ,(YVU444      , "YV24")
         ]

-- | Bit-depth per plane
formatBitDepth :: Format -> [Int]
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
   deriving (Eq,Ord,Show)
