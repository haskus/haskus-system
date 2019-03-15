{-# LANGUAGE TypeApplications #-}

-- | Generative art
--
-- Taken from: https://www.reddit.com/r/haskell/comments/8336bq/generating_artwork_with_haskell/
module Demo.Art
   ( makeArt
   )
where

import Haskus.System.Graphics.Drawing

import Codec.Picture            (Image)
import Control.Arrow
import Control.Monad.Random
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.List                (nub)
import Data.Semigroup           ((<>))
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Transformations
import qualified Numeric.Noise.Perlin     as P

makeArt :: Int -> Int -> Int -> Float -> Image PixelRGBA8
makeArt seed width height scaling = run world stdGen (render sketch)
   where
      stdGen  = mkStdGen seed
      world   = World width height seed scaling eggShell

data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , _worldScale  :: Float
  , _bgColor     :: PixelRGBA8
  }

type Generate a = RandT StdGen (Reader World) a

run :: World -> StdGen -> Generate a -> a
run w g gen = runReader (evalRandT gen g) w

hsva :: Double -> Double -> Double -> Double -> PixelRGBA8
hsva h s v a = PixelRGBA8 (w8 $ rC) (w8 gC) (w8 bC) (w8 a)
  where
    RGB rC gC bC = hsv h s v
    w8 x = round (255 * x)

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

render :: Generate (Drawing PixelRGBA8 ()) -> Generate (Image PixelRGBA8)
render d = do
  d' <- d
  World w h _ s bg <- ask
  let w' = round $ fromIntegral w * s
      h' = round $ fromIntegral h * s
  return $ renderDrawing w' h' bg (withTransformation (scale s s) d')

--------------------------------------------------------------------------------

eggShell, darkGunMetal, teaGreen, vividTangerine, englishVermillion :: PixelRGBA8
eggShell          = hsva 71  0.13 0.96 1.0
darkGunMetal      = hsva 170 0.30 0.16 1.0
teaGreen          = hsva 81  0.25 0.94 1.0
vividTangerine    = hsva 11  0.40 0.92 1.0
englishVermillion = hsva 355 0.68 0.84 1.0

data Quad = Quad
  { _quadA :: V2 Float
  , _quadB :: V2 Float
  , _quadC :: V2 Float
  , _quadD :: V2 Float
  } deriving (Eq)

fromIntegralVector :: V2 Int -> V2 Float
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

genQuadGrid :: Generate [Quad]
genQuadGrid = do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in Quad v' (v' ^+^ V2 0 1.5) (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 0)

closedPath :: [V2 Float] -> [Line]
closedPath []  = []
closedPath [p] = [Line p p]
closedPath qs@(p1:p2:ps) = zipWith Line qs (p2:ps <> [p1])

quad :: Quad -> [Line]
quad (Quad a b c d) = closedPath [a, b, c, d]

quadAddNoise :: Quad -> Generate Quad
quadAddNoise (Quad a b c d) = do
  perlinSeed <- fromIntegral <$> asks worldSeed

  let
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistance = 0.5
    perlinNoise
      = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (V2 x y)
      = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

  pure $ Quad
    (realToFrac <$> addNoise (realToFrac <$> a))
    (realToFrac <$> addNoise (realToFrac <$> b))
    (realToFrac <$> addNoise (realToFrac <$> c))
    (realToFrac <$> addNoise (realToFrac <$> d))

sketchQuad :: [Line] -> Generate (Drawing PixelRGBA8 ())
sketchQuad ls = do
  stroked <- weighted [(True, 0.6), (False, 0.4)]
  color <- uniform
    [ teaGreen
    , vividTangerine
    , englishVermillion
    , darkGunMetal
    ]
  return $ withTexture (uniformTexture color)
         $ if stroked
             then stroke 0.15 (JoinMiter 0) (CapStraight 0, CapStraight 0) ls
             else fill ls

sketch :: Generate (Drawing PixelRGBA8 ())
sketch = do
  quads      <- genQuadGrid
  noisyQuads <- traverse quadAddNoise quads
  mconcat <$> traverse (sketchQuad . quad) noisyQuads
