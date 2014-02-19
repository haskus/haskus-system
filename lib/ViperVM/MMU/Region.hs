-- | A region is the shape of a set of memory cells in a buffer with an offset
module ViperVM.MMU.Region(
   Region(..), Offset,
   regionOffset, regionsWithSameShape, overlapsAny, overlaps,
   regionCover
) where

import Data.Word

type Offset = Word64    -- ^ Offset in bytes
type Size = Word64      -- ^ Size in bytes
type Padding = Word64   -- ^ Padding (may be changed to a smaller type in the future)
type RowCount = Word64  -- ^ Number of rows

-- | Shape of a set of memory cells at a given offset
data Region = 
     Region1D Offset Size                    -- ^ Contiguous set of cells
   | Region2D Offset RowCount Size Padding   -- ^ Rectangular set of cells
   deriving (Eq,Ord,Show)

-- | Check if two regions have the same shape (discarding offset and padding)
regionsWithSameShape :: Region -> Region -> Bool
regionsWithSameShape (Region1D _ s1) (Region1D _ s2) = s1 == s2
regionsWithSameShape (Region2D _ idx1 s1 _) (Region2D _ idx2 s2 _) = (s1 == s2) && (idx1 == idx2)
regionsWithSameShape _ _ = False

-- | Return region offset in buffer
regionOffset :: Region -> Offset
regionOffset (Region1D off _) = off
regionOffset (Region2D off _ _ _) = off


-- | Return covering 1D region
regionCover :: Region -> Region
regionCover r@(Region1D {}) = r
regionCover (Region2D off nrows sz pad) = Region1D off (nrows * (sz+pad))

-- | Retrieve regions that overlap with the given region
overlapsAny :: Region -> [Region] -> [Region]
overlapsAny reg = filter (overlaps reg)

-- | Indicate if two regions overlap (may return false positive)
overlaps :: Region -> Region -> Bool
overlaps (Region1D off1 sz1) (Region1D off2 sz2) = not (left1 >= right2 || left2 >= right1)
   where
      left1 = off1
      left2 = off2
      right1 = off1+sz1
      right2 = off2+sz2

overlaps (Region1D off sz) r2 = overlaps (Region2D off 1 sz 0) r2
overlaps r2 (Region1D off sz) = overlaps r2 (Region2D off 1 sz 0)

overlaps (Region2D off1 rc1 sz1 pad1) (Region2D off2 rc2 sz2 pad2) | sz1+pad1 == sz2+pad2 = 
   not ( left1 >= right2 || left2 >= right1 || top1 >= bottom2 || top2 >= bottom1 )
   where
      width = sz1+pad1
      (top1,left1) = divMod off1 width
      (top2,left2) = divMod off2 width
      right1 = left1 + sz1
      right2 = left2 + sz2
      bottom1 = top1 + rc1
      bottom2 = top2 + rc2

-- FIXME: by default we say that 2 regions Region2D overlap if they don't have the same padding.
-- Maybe we could improve this if we find a fast algorithm
overlaps _ _ = True
