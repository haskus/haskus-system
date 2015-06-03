module ViperVM.Format.Binary.BitOrder
   ( BitOrder(..)
   )
where

-- | Bit order
--
-- E.g. two words of 5 bits: ABCDE, VWXYZ
--    - BB: ABCDEVWX YZxxxxxx
--    - LL: XYZABCDE xxxxxxVW
--    - BL: EDCBAZYX WVxxxxxx
--    - LB: XWVEDCBA xxxxxxZY
data BitOrder
   = BB  -- ^ Big-endian bytes and bits
   | LB  -- ^ Little-endian bytes, big-endian bits
   | BL  -- ^ Big-endian bytes, little-endian bits
   | LL  -- ^ Little-endian bytes and bits
   deriving (Show,Eq)
