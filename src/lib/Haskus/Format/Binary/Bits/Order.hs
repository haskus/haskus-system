-- | Bit orderings
module Haskus.Format.Binary.Bits.Order
   ( BitOrder(..)
   )
where

-- | Bit order
--
-- The first letter indicates the outer bit ordering, i.e. how bytes are filled:
--    B*: from left to right (B is for BigEndian)
--    L*: from right to left (L is for LittleEndian)
--
-- The second letter indicates the inner bit ordering, i.e. how words are stored:
--    *B: the most significant bit is stored first (in the outer bit order!)
--    *L: the least-significant bit is stored first (in the outer bit order!)
--
-- E.g. two successive words of 5 bits: ABCDE, VWXYZ
--    - BB: ABCDEVWX YZxxxxxx
--    - BL: EDCBAZYX WVxxxxxx
--    - LB: XWVEDCBA xxxxxxZY
--    - LL: XYZABCDE xxxxxxVW
data BitOrder
   = BB
   | LB
   | BL
   | LL
   deriving (Show,Eq)
