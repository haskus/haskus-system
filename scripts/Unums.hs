{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Haskus.Binary.Unum
import Data.Proxy

data PI

instance UnumNum PI where
   unumLabel _ = "Pi"

type U0 = Unum '[]
type U1 = Unum '[I 1]
type U2 = Unum '[I 1, I 2] -- incomplete system
type U3 = Unum '[I 1, I 2, I 3]
type U4 = Unum '[I 1, I 2, I 3, I 4]

type UPI = Unum '[I 1, I 2, PI, I 4]

u0 :: U U2
u0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 0)) ExactNumber

u0' :: U U2
u0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 0)) OpenInterval

u1 :: U U2
u1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 1)) ExactNumber

u1' :: U U2
u1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 1)) OpenInterval

u2 :: U U2
u2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 2)) ExactNumber

u2' :: U U2
u2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 2)) OpenInterval

r0 :: U U2
r0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 0))) ExactNumber

r0' :: U U2
r0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 0))) OpenInterval

r1 :: U U2
r1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 1))) ExactNumber

r1' :: U U2
r1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 1))) OpenInterval

r2 :: U U2
r2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 2))) ExactNumber

r2' :: U U2
r2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 2))) OpenInterval

n0 :: U U2
n0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 0))) ExactNumber

n0' :: U U2
n0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 0))) OpenInterval

n1 :: U U2
n1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 1))) ExactNumber

n1' :: U U2
n1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 1))) OpenInterval

n2 :: U U2
n2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 2))) ExactNumber

n2' :: U U2
n2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 2))) OpenInterval

nr0 :: U U2
nr0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 0)))) ExactNumber

nr0' :: U U2
nr0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 0)))) OpenInterval

nr1 :: U U2
nr1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 1)))) ExactNumber

nr1' :: U U2
nr1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 1)))) OpenInterval

nr2 :: U U2
nr2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 2)))) ExactNumber

nr2' :: U U2
nr2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 2)))) OpenInterval

instance SornAdd U2 where
   sornAddU x y
      -- infinite
      | x == r0 && y == r0    = sornFull
      | x == r0               = sornSingle r0
      -- 0
      | x == u0               = sornSingle y
      -- negatives
      | unumSign x == Negative
        && unumSign y == Negative = sornNegate (sornAddU (unumNegate x) (unumNegate y))
      -- 0..
      | x == u0' && y == r0'   = sornFromTo r0' n2'
      | x == u0' && y == n2    = sornSingle n2'
      | x == u0' && y == n2'   = sornFromTo n2' n1'
      | x == u0' && y == n1    = sornSingle n1'
      | x == u0' && y == n1'   = sornFromTo n1' nr2'
      | x == u0' && y == nr2   = sornSingle nr2'
      | x == u0' && y == nr2'  = sornFromTo nr2' u0'
      | x == u0' && y == u0'   = sornFromTo u0' r2'
      | x == u0' && y == r2    = sornSingle r2'
      | x == u0' && y == r2'   = sornFromTo r2' u1'
      | x == u0' && y == u1    = sornSingle u1'
      | x == u0' && y == u1'   = sornFromTo u1' u2'
      | x == u0' && y == u2    = sornSingle u2'
      | x == u0' && y == u2'   = sornSingle u2'
      -- /2
      | x == r2 && y == r0'   = sornFromTo r0' n2'
      | x == r2 && y == n2    = sornSingle n2'
      | x == r2 && y == n2'   = sornFromTo n2' n1'
      | x == r2 && y == n1    = sornSingle nr2
      | x == r2 && y == n1'   = sornSingle nr2'
      | x == r2 && y == nr2   = sornSingle u0
      | x == r2 && y == nr2'  = sornSingle u0'
      | x == r2 && y == u0'   = sornSingle r2'
      | x == r2 && y == r2    = sornSingle u1
      | x == r2 && y == r2'   = sornSingle u1'
      | x == r2 && y == u1    = sornSingle u1'
      | x == r2 && y == u1'   = sornFromTo u1' u2'
      | x == r2 && y == u2    = sornSingle u2'
      | x == r2 && y == u2'   = sornSingle u2'
      -- /2..
      | x == r2' && y == r0'   = sornFromTo r0' n2'
      | x == r2' && y == n2    = sornSingle n2'
      | x == r2' && y == n2'   = sornFromTo n2' nr2'
      | x == r2' && y == n1    = sornSingle nr2'
      | x == r2' && y == n1'   = sornFromTo nr2' u0'
      | x == r2' && y == nr2   = sornSingle u0'
      | x == r2' && y == nr2'  = sornFromTo u0' r2'
      | x == r2' && y == u0'   = sornFromTo r2' u1'
      | x == r2' && y == r2    = sornSingle u1'
      | x == r2' && y == r2'   = sornSingle u1'
      | x == r2' && y == u1    = sornSingle u1'
      | x == r2' && y == u1'   = sornFromTo u1' u2'
      | x == r2' && y == u2    = sornSingle u2'
      | x == r2' && y == u2'   = sornSingle u2'
      -- 1
      | x == u1 && y == r0'   = sornFromTo r0' n2'
      | x == u1 && y == n2    = sornSingle n1
      | x == u1 && y == n2'   = sornFromTo n1' nr2'
      | x == u1 && y == n1    = sornSingle u0
      | x == u1 && y == n1'   = sornSingle u0'
      | x == u1 && y == nr2   = sornSingle r2
      | x == u1 && y == nr2'  = sornSingle r2'
      | x == u1 && y == u0'   = sornSingle u1'
      | x == u1 && y == r2    = sornSingle u1'
      | x == u1 && y == r2'   = sornSingle u1'
      | x == u1 && y == u1    = sornSingle u2
      | x == u1 && y == u1'   = sornSingle u2'
      | x == u1 && y == u2    = sornSingle u2'
      | x == u1 && y == u2'   = sornSingle u2'
      -- 1..
      | x == u1' && y == r0'   = sornFromTo r0' n1'
      | x == u1' && y == n2    = sornFromTo n1' nr2'
      | x == u1' && y == n2'   = sornFromTo n1' r2'
      | x == u1' && y == n1    = sornFromTo u0' r2'
      | x == u1' && y == n1'   = sornFromTo u0' u1'
      | x == u1' && y == nr2   = sornFromTo r2' u1'
      | x == u1' && y == nr2'  = sornFromTo r2' u1'
      | x == u1' && y == u0'   = sornFromTo u1' u2'
      | x == u1' && y == r2    = sornFromTo u1' u2'
      | x == u1' && y == r2'   = sornFromTo u1' u2'
      | x == u1' && y == u1    = sornSingle u2'
      | x == u1' && y == u1'   = sornSingle u2'
      | x == u1' && y == u2    = sornSingle u2'
      | x == u1' && y == u2'   = sornSingle u2'
      -- 2
      | x == u2 && y == r0'   = sornFromTo r0' nr2'
      | x == u2 && y == n2    = sornSingle u0
      | x == u2 && y == n2'   = sornFromTo u0' r2'
      | x == u2 && y == n1    = sornSingle u1
      | x == u2 && y == n1'   = sornSingle u1'
      | x == u2 && y == nr2   = sornSingle u1'
      | x == u2 && y == nr2'  = sornSingle u1'
      | x == u2 && y == u0'   = sornSingle u2'
      | x == u2 && y == r2    = sornSingle u2'
      | x == u2 && y == r2'   = sornSingle u2'
      | x == u2 && y == u1    = sornSingle u2'
      | x == u2 && y == u1'   = sornSingle u2'
      | x == u2 && y == u2    = sornSingle u2'
      | x == u2 && y == u2'   = sornSingle u2'
      -- 2..
      | x == u2' && y == r0'   = sornNonInfinite
      | x == u2' && y == n2    = sornFromTo u0' u2'
      | x == u2' && y == n2'   = sornFromTo u0' u2'
      | x == u2' && y == n1    = sornFromTo u1' u2'
      | x == u2' && y == n1'   = sornFromTo u1' u2'
      | x == u2' && y == nr2   = sornFromTo u1' u2'
      | x == u2' && y == nr2'  = sornFromTo u1' u2'
      | x == u2' && y == u0'   = sornSingle u2'
      | x == u2' && y == r2    = sornSingle u2'
      | x == u2' && y == r2'   = sornSingle u2'
      | x == u2' && y == u1    = sornSingle u2'
      | x == u2' && y == u1'   = sornSingle u2'
      | x == u2' && y == u2    = sornSingle u2'
      | x == u2' && y == u2'   = sornSingle u2'

      -- "add" commutes
      | otherwise              = sornAddU y x
