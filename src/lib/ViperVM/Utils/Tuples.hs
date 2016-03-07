module ViperVM.Utils.Tuples
   ( uncurry4
   , take4
   , fromTuple4
   )
where

-- | Uncurry specialised for quadruple
uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

{-# INLINE uncurry4 #-}

-- | Take specialised for quadruple
take4 :: [a] -> (a,a,a,a)
take4 [a,b,c,d] = (a,b,c,d)
take4 _         = error "take4: invalid list (exactly 4 elements required)"

{-# INLINE take4 #-}

-- | toList for quadruple
fromTuple4 :: (a,a,a,a) -> [a]
fromTuple4 (a,b,c,d) = [a,b,c,d]

{-# INLINE fromTuple4 #-}
