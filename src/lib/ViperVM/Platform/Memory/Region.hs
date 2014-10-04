{-# LANGUAGE PatternSynonyms #-}

-- | A region represents a set of memory cells in a buffer.
--
-- Each region has an offset (the offset of the first cell in the buffer) and a
-- /shape/ that describes which cells are part of the region. The aim is to
-- have a lightweight representation of the set of memory cells that can be
-- used to perform transfers between memories.
--
-- Currently two region shapes are supported:
--    * Shape1D: region without hole (contiguous set of cells)
--    * Shape2D: rectangular set of cells (e.g. sub-array), that is with a
--    constant number of padding bytes after each row
--
module ViperVM.Platform.Memory.Region
   (
   -- * Shapes
     Shape(..)
   , Width
   , Height
   , Padding
   , shapeSimplify
   , shapeCover
   , isHomomorphicWith
   -- * Regions
   , Region(..)
   , pattern Region1D
   , pattern Region2D
   , overlapsAny
   , overlaps
   , regionCover1D
   , regionCoverIntersection
   )
where

import Data.Word
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isNothing)

-- | Width in bytes
type Width = Word64      

-- | Number of rows
type Height = Word64
   
-- | Padding (may be changed to a smaller type in the future)
type Padding = Word64


-- | Shape of a set of memory cells at a given offset
data Shape
   = Shape1D Width                    -- ^ Contiguous set of cells
   | Shape2D Height Width Padding   -- ^ Rectangular set of cells
   deriving (Eq,Ord,Show)

-- | Positioned region (i.e. region shape with an offset)
data Region = Region
   { regionOffset :: Word64          -- ^ Offset of the first cell
   , regionShape  :: Shape           -- ^ Shape of the region
   }

-- | Pattern synonym for region with 1D shape
pattern Region1D o w = Region o (Shape1D w)

-- | Pattern synonym for region with 2D shape
pattern Region2D o h w p = Region o (Shape2D h w p)


-- | Check if two regions have the same shape (discarding padding)
isHomomorphicWith :: Shape -> Shape -> Bool
isHomomorphicWith r1 r2 = case (shapeSimplify r1, shapeSimplify r2) of
   (Shape1D w1, Shape1D w2)           -> w1 == w2
   (Shape2D h1 w1 _, Shape2D h2 w2 _) -> (w1 == w2) && (h1 == h2)
   _                                  -> False

-- | Try to simplify the shape (e.g. transform a 2D region into a 1D one)
shapeSimplify :: Shape -> Shape
shapeSimplify (Shape2D h w 0) = Shape1D (h*w)
shapeSimplify (Shape2D 1 w _) = Shape1D w
shapeSimplify r               = r

-- | Return covering 1D shape
shapeCover :: Shape -> Shape
shapeCover r@(Shape1D {}) = r
shapeCover (Shape2D h w p) = Shape1D (h * (w+p) - p)

-- | Return covering 1D region
regionCover1D :: Region -> Region
regionCover1D (Region off shape) = Region off (shapeCover shape)

-- | Return intersection of both covering regions, if any
regionCoverIntersection :: Region -> Region -> Maybe Region
regionCoverIntersection r1 r2 = 
   if not (overlaps c1 c2)
      then Nothing
      else Just (Region1D (max o1 o2) (min (o1+w1) (o2+w2)))
   where
      c1@(Region1D o1 w1) = regionCover1D r1
      c2@(Region1D o2 w2) = regionCover1D r2


-- | Retrieve regions that overlap with the given region
overlapsAny :: Region -> [Region] -> [Region]
overlapsAny reg = filter (overlaps reg)

-- | Indicate if two regions overlap (may return false positive)
overlaps :: Region -> Region -> Bool
overlaps r1 r2 =
   case (r1,r2) of
      -- Two 1D regions
      (Region1D o1 w1, Region1D o2 w2) -> not (o1 >= o2+w2 || o2 >= o1+w1)

      -- Try with covering 1D regions, if they do not overlap, r1 and r2 neither do
      _ | isNothing (regionCoverIntersection r1 r2) -> False

      -- Transform the 1D region into a 2D one
      (Region1D o w,_)     -> overlaps (Region2D o 1 w 0) r2
      (_, Region1D o w)    -> overlaps r1 (Region2D o 1 w 0)

      -- 2D regions case
      -- FIXME: add test on h1,h2,o1,o2 if any (uncurry overlaps) rs is true
      -- In some cases, it is a false positive
      (Region2D o1 h1 w1 p1, Region2D o2 h2 w2 p2) -> any (uncurry overlaps) rs
         where
            period = lcm (w1+p1) (w2+p2)

            -- We create a mask for the period (mask = invalid cells in period)
            Just (Region1D interOff interSize) = regionCoverIntersection r1 r2
            (maskStart,maskWidth) = 
               if interSize >= period 
                  -- The intersection covers a whole period
                  then (0,0)
                  -- Otherwise
                  else  ((interOff + interSize) `mod` period, period-interSize)

            -- all combinations of lines from r1 and r2 in a period
            rs = (,) <$> wrap o1 w1 p1 period <*> wrap o2 w2 p2 period

            -- Fill a space of size s with 1D regions from each line of the given region.
            -- s must be a multiple of w+p
            wrap o w p s = regs
               where
                  m = s `div` (w+p)
                  -- offset in Z/Zs
                  d = o `mod` s
                  -- Line truncation:
                  --   * wr last bytes of the first line
                  --   * wl first bytes of the last line
                  wr = d `mod` (w+p)
                  wl = w + p - wr
                  -- First line
                  fl = if wr > p then [Region1D 0 (wr-p)] else []
                  -- Last line
                  ll = if wl > 0 then [Region1D (s-wl) (min wl w)] else []
                  -- 1D regions for each row
                  regs = fl ++ ll ++ [Region1D (wr+d') w | t <- [0..m-1], let d' = t * (w+p)]
