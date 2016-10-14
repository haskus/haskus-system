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

import ViperVM.Format.Binary.Word
import ViperVM.Utils.Maybe (isJust,mapMaybe)

-- | Width in bytes
type Width = Word64      

-- | Number of rows
type Height = Word64
   
-- | Padding (may be changed to a smaller type in the future)
type Padding = Word64


-- | Shape of a set of memory cells at a given offset
data Shape
   = Shape1D Width                  -- ^ Contiguous set of cells
   | Shape2D Height Width Padding   -- ^ Rectangular set of cells
   deriving (Eq,Ord,Show)

-- | Positioned region (i.e. region shape with an offset)
data Region = Region
   { regionOffset :: Word64          -- ^ Offset of the first cell
   , regionShape  :: Shape           -- ^ Shape of the region
   } deriving (Show)

-- | Pattern synonym for region with 1D shape
pattern Region1D :: Word64 -> Width -> Region
pattern Region1D o w = Region o (Shape1D w)

-- | Pattern synonym for region with 2D shape
pattern Region2D :: Word64 -> Height -> Width -> Padding -> Region
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
      else let o = max o1 o2 in Just (Region1D o (min (o1+w1) (o2+w2) - o))
   where
      c1@(Region1D o1 w1) = regionCover1D r1
      c2@(Region1D o2 w2) = regionCover1D r2


-- | Retrieve regions that overlap with the given region
overlapsAny :: Region -> [Region] -> [Region]
overlapsAny reg = filter (overlaps reg)

-- | Indicate if two regions overlap (may return false positive)
overlaps :: Region -> Region -> Bool

-- Two 1D regions
overlaps (Region1D o1 w1) (Region1D o2 w2) = not (o1 >= o2+w2 || o2 >= o1+w1)

-- Transform the 1D region into a 2D one
overlaps (Region1D o w) r2 = overlaps (Region2D o 1 w 0) r2
overlaps r1 (Region1D o w) = overlaps r1 (Region2D o 1 w 0)
      
-- 2D regions case
overlaps r1@(Region2D o1 _ w1 p1) r2@(Region2D o2 _ w2 p2) = 
   -- Try with covering 1D regions, if they do not overlap, r1 and r2
   -- neither do
    isJust (regionCoverIntersection r1 r2) && not (null inters)
   where
      -- cycle-width
      cw = lcm (w1+p1) (w2+p2)

      -- number of lines in the cycle
      m1 = cw `div` (w1+p1)
      m2 = cw `div` (w2+p2)

      -- relative offset 
      ro1 = o1 `mod` (w1+p1)
      ro2 = o2 `mod` (w2+p2)

      -- compute the line of a cycle
      regionLines m w p ro
         | ro <= p    = [Region1D (ro + k * (w+p)) w | k <- [0..m-1]]
         | otherwise  = fs : ls : os
            where
               fs = Region1D 0 (ro-p)
               ls = Region1D (cw-(w+p)+ro) (w+p-ro)
               os = [Region1D (ro + k * (w+p)) w | k <- [0..m-2]]

      lines1 = regionLines m1 w1 p1 ro1
      lines2 = regionLines m2 w2 p2 ro2

      -- now we need to filter false positives
      Just (Region1D c1 icw) = regionCoverIntersection r1 r2
      c2 = icw + c1 - 1

      rc1 = c1 `mod` cw
      rc2 = c2 `mod` cw

      -- line-filter
      ff = case (icw >= cw, rc1 <= rc2) of
         (True, _)    -> id
         (False,True) -> mapMaybe (regionCoverIntersection (Region1D rc1 icw))
         (False,False) -> \ls -> k vz1 ls ++ k vz2 ls
            where 
               k x = mapMaybe (regionCoverIntersection x)
               vz1 = Region1D 0 rc2
               vz2 = Region1D rc1 (cw-rc1)
                       
      -- couple of lines intersecting
      inters = [(x,y) | x <- ff lines1, y <- ff lines2, overlaps x y]

-- In theory I think we handle all of the cases, but GHC complains the "_ _"
-- case is missing...
overlaps _ _ = undefined
