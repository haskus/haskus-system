{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Graphics scene
module ViperVM.System.Graphics.Scene
   ( Scene (..)
   , Object (..)
   , insertObject
   , renderScene
   , ClipRect (..)
   , ObjectOp (..)
   -- * Objects
   , DefaultObjects
   , Rectangle (..)
   , Bitmap (..)
   )
where

import ViperVM.Utils.STM.TMap as TMap
import ViperVM.Utils.Variant
import ViperVM.Utils.STM
import ViperVM.Utils.List
import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.System.Graphics.Colour

import Control.Loop (forLoop)

-- | A scene
data Scene objs name = Scene
   { sceneObjects  :: TMap name (Object objs) -- ^ Objects in the scene
   , sceneObjId    :: TVar Word               -- ^ Next available object Identifier
   , sceneViewPort :: TVar ViewPort           -- ^ View port
   }

-- | An object in the scene
--
-- The object position is in 2D World coordinates
data Object (o :: [*]) = Object
   { objectWidth  :: {-# UNPACK #-} !Float       -- ^ Width in World coordinates
   , objectHeight :: {-# UNPACK #-} !Float       -- ^ Height in World coordinates
   , objectX      :: {-# UNPACK #-} !Float       -- ^ X in World coordinates
   , objectY      :: {-# UNPACK #-} !Float       -- ^ Y in World coordinates
   , objectZ      :: {-# UNPACK #-} !Float       -- ^ Z in World coordinates
   , objectData   :: {-# UNPACK #-} !(Variant o) -- ^ The object
   , objectId     :: {-# UNPACK #-} !Word        -- ^ Object internal indentifier
   }
   
deriving instance Show (Variant o) => Show (Object o)

-- | A view port is a window on the World
--
-- To avoid deformation, it must have the same ratio as the screen
data ViewPort = ViewPort
   { viewWidth  :: {-# UNPACK #-} !Float  -- ^ Width in World coordinates
   , viewHeight :: {-# UNPACK #-} !Float  -- ^ Height in World coordinates
   , viewX      :: {-# UNPACK #-} !Float  -- ^ X in World coordinates
   , viewY      :: {-# UNPACK #-} !Float  -- ^ Y in World coordinates
   , viewZ      :: {-# UNPACK #-} !Float  -- ^ Z in World coordinates
   } deriving (Show)


-- | A bitmap plane
--
-- This indicates where to render in memory.
data Plane = Plane
   { planeAddr   :: Ptr ()
   , planePixel  :: PixelFormat
   , planeWidth  :: Word
   , planeHeight :: Word
   , planePitch  :: Word
   }


-- | Insert an object in a scene
insertObject ::
   ( Member obj objs
   , Key name
   ) => Scene objs name -> name -> obj -> STM ()
insertObject sc name obj = do
   n <- readTVar (sceneObjId sc)
   writeTVar (sceneObjId sc) (n+1)
   TMap.insert name (Object 0 0 10 10 1 (setVariant obj) n) (sceneObjects sc)


-- | Render a scene
renderScene :: Scene objs name -> Plane -> IO ()
renderScene sc view plane = do
   objs <- atomically $ TMap.elems (sceneObjects sc)

   -- sort objects by z-order
   let objs' = sortOn objectZ objs

   -- forM_ objs' $ \obj -> do
   --    let P
   --    -- determine clip rects
   --    let dstRect = translate clip' pos `intersect` frame
   --    let srcRect = translate dstRect (-1 * px, -1 * py)
   -- 
   --    -- draw object
   --    drawObject obj destPlane clip

   return ()


data ClipRect = ClipRect
   { clipWidth  :: Word
   , clipHeight :: Word
   , clipX      :: Word
   , clipY      :: Word
   }

class ObjectsOp objs where
   drawObject' :: Variant objs -> Plane -> ClipRect -> IO ()

instance ObjectsOp '[] where
   drawObject' = undefined

instance
   ( ObjectsOp xs
   , ObjectOp x
   ) => ObjectsOp (x ': xs) where
   drawObject' v plane clip = case headVariant v of
      Right x -> drawObject x plane clip
      Left xs -> drawObject' xs plane clip

class ObjectOp o where
   drawObject :: o -> Plane -> ClipRect -> IO ()

instance ObjectOp Rectangle where
   drawObject r plane clip = do
      let 
         v  = rectColor r
      forLoop 0 (< clipHeight clip) (+1) $! \y -> do
         let dx = y*planePitch plane
         forLoop 0 (< clipWidth clip) (+1) $! \x -> do
            -- dest offset
            let !doff = x*4 + dx
            pokeByteOff (castPtr (planeAddr plane)) (fromIntegral doff) v

-- | Default list of supported objects
type DefaultObjects =
   '[ Rectangle
    , Bitmap
    ]

data Material
   = Matte Colour


-- | A rectangle
data Rectangle = Rectangle
   { rectOriginalMaterial :: Material
   }

-- | A bitmap image
data Bitmap = Bitmap
   { imagePixel  :: PixelFormat
   , imageData   :: Buffer
   }
