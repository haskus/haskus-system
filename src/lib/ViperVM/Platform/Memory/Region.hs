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
   , Size
   , Padding
   , RowCount
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
   )
where

import Data.Word

-- | Size in bytes
type Size = Word64      
   
-- | Padding (may be changed to a smaller type in the future)
type Padding = Word64

-- | Number of rows
type RowCount = Word64

-- | Shape of a set of memory cells at a given offset
data Shape
   = Shape1D Size                    -- ^ Contiguous set of cells
   | Shape2D RowCount Size Padding   -- ^ Rectangular set of cells
   deriving (Eq,Ord,Show)

-- | Positioned region (i.e. region shape with an offset)
data Region = Region
   { regionOffset :: Word64          -- ^ Offset of the first cell
   , regionShape  :: Shape           -- ^ Shape of the region
   }

-- | Pattern synonym for region with 1D shape
pattern Region1D off sz = Region off (Shape1D sz)

-- | Pattern synonym for region with 2D shape
pattern Region2D off nrows sz padding = Region off (Shape2D nrows sz padding)


-- | Check if two regions have the same shape (discarding padding)
isHomomorphicWith :: Shape -> Shape -> Bool
isHomomorphicWith r1 r2 = case (shapeSimplify r1, shapeSimplify r2) of
   (Shape1D s1, Shape1D s2)               -> s1 == s2
   (Shape2D idx1 s1 _, Shape2D idx2 s2 _) -> (s1 == s2) && (idx1 == idx2)
   _                                      -> False

-- | Try to simplify the shape (e.g. transform a 2D region into a 1D one)
shapeSimplify :: Shape -> Shape
shapeSimplify (Shape2D idx s 0) = Shape1D (idx*s)
shapeSimplify (Shape2D 1 s _)   = Shape1D s
shapeSimplify r                 = r

-- | Return covering 1D shape
shapeCover :: Shape -> Shape
shapeCover r@(Shape1D {}) = r
shapeCover (Shape2D nrows sz pad) = Shape1D (nrows * (sz+pad))

-- | Return covering 1D region
regionCover1D :: Region -> Region
regionCover1D (Region off shape) = Region off (shapeCover shape)

-- | Retrieve regions that overlap with the given region
overlapsAny :: Region -> [Region] -> [Region]
overlapsAny reg = filter (overlaps reg)

-- | Indicate if two regions overlap (may return false positive)
overlaps :: Region -> Region -> Bool
overlaps r1 r2 =
   case (r1,r2) of
      -- Two 1D regions
      (Region1D off1 sz1, Region1D off2 sz2) -> not (left1 >= right2 || left2 >= right1)
            where 
               left1 = off1
               left2 = off2
               right1 = off1+sz1
               right2 = off2+sz2

      -- Try with covering 1D regions, if they do not overlap, r1 and r2 neither do
      _ | not (overlaps (regionCover1D r1) (regionCover1D r2)) -> False

      -- Transform the 1D region into a 2D one
      (Region1D off sz,_)     -> overlaps (Region2D off 1 sz 0) r2
      (_, Region1D off sz)    -> overlaps r1 (Region2D off 1 sz 0)

      -- Simple 2D case: rectangular regions
      (Region2D o1 h1 w1 p1, Region2D o2 h2 w2 p2) | w1+p1 == w2+p2 -> 
         not ( left1 >= right2 || left2 >= right1 || top1 >= bottom2 || top2 >= bottom1 )
            where
               width = w1+p1
               (top1,left1) = divMod o1 width
               (top2,left2) = divMod o2 width
               right1 = left1 + w1
               right2 = left2 + w2
               bottom1 = top1 + h1
               bottom2 = top2 + h2

      -- General case
      -- FIXME: add test on h1,h2,o1,o2 if any (uncurry overlaps) rs is true
      -- In some cases, it is a false positive
      (Region2D o1 h1 w1 p1, Region2D o2 h2 w2 p2) -> any (uncurry overlaps) rs
         where
            -- w == m1 * (w1+p1) == m2 * (w2+p2)
            w = lcm (w1+p1) (w2+p2)
            m1 = w `div` (w1+p1)
            m2 = w `div` (w2+p2)
            -- offsets in Z/Zw
            d1 = o1 `mod` w
            d2 = o2 `mod` w
            -- Couple of 1D regions per row in each cycle
            -- FIXME: as we shift (cf d1,d2), we need to wrap rows at the beginning
            -- potentially splitting w1/w2 of the first/last row
            rs = [(Region1D (d1+d1') w1, Region1D (d2+d2') w2)
                     | t1 <- [0..m1-1]
                     , t2 <- [0..m2-1]
                     , let d1' = t1 * (w1+p1)
                     , let d2' = t2 * (w2+p2)]
