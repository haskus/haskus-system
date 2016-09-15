{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ViperVM.Format.Binary.Unum
import Data.Proxy

data PI

instance UnumNum PI where
   unumLabel _ = "Pi"

type U0 = Unum '[]
type U1 = Unum '[I 1]
type U2 = Unum '[I 1, I 2]
type U3 = Unum '[I 1, I 2, I 3]
type U4 = Unum '[I 1, I 2, I 3, I 4]

type UPI = Unum '[I 1, I 2, PI, I 4]

u0 :: U U2
u0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 0)) Number

u0' :: U U2
u0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 0)) Range

u1 :: U U2
u1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 1)) Number

u1' :: U U2
u1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 1)) Range

u2 :: U U2
u2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 2)) Number

u2' :: U U2
u2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (I 2)) Range

r0 :: U U2
r0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 0))) Number

r0' :: U U2
r0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 0))) Range

r1 :: U U2
r1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 1))) Number

r1' :: U U2
r1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 1))) Range

r2 :: U U2
r2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 2))) Number

r2' :: U U2
r2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (I 2))) Range

n0 :: U U2
n0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 0))) Number

n0' :: U U2
n0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 0))) Range

n1 :: U U2
n1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 1))) Number

n1' :: U U2
n1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 1))) Range

n2 :: U U2
n2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 2))) Number

n2' :: U U2
n2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Neg (I 2))) Range

nr0 :: U U2
nr0 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 0)))) Number

nr0' :: U U2
nr0' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 0)))) Range

nr1 :: U U2
nr1 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 1)))) Number

nr1' :: U U2
nr1' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 1)))) Range

nr2 :: U U2
nr2 = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 2)))) Number

nr2' :: U U2
nr2' = unumEncode (Proxy :: Proxy U2) (Proxy :: Proxy (Rcp (Neg (I 2)))) Range

instance SornAdd U2 where
   sornAddU x y
      -- negatives
      | unumSign x == Negative
        && unumSign y == Negative = sornNegate (sornAddU (unumNegate x) (unumNegate y))
      -- infinite
      | x == r0 && y == r0    = sornFull
      | x == r0               = sornSingle r0
      -- 0
      | x == u0               = sornSingle y
      -- 0..
      | x == u0' && y == r0'   = sornFromElems [r0',n2,n2']
      | x == u0' && y == n2    = sornSingle n2'
      | x == u0' && y == n2'   = sornFromElems [n2', n1, n1']
      | x == u0' && y == n1    = sornSingle n1'
      | x == u0' && y == n1'   = sornFromElems [n1',nr2,nr2']
      | x == u0' && y == nr2   = sornSingle nr2'
      | x == u0' && y == nr2'  = sornFromElems [nr2',u0,u0']
      | x == u0' && y == u0'   = sornFromElems [u0',r2,r2']
      | x == u0' && y == r2    = sornSingle r2'
      | x == u0' && y == r2'   = sornFromElems [r2',u1,u1'] 
      | x == u0' && y == u1    = sornSingle u1'
      | x == u0' && y == u1'   = sornFromElems [u1', u2, u2']
      | x == u0' && y == u2    = sornSingle u2'
      | x == u0' && y == u2'   = sornSingle u2'
      -- /2
      | x == r2 && y == r0'   = sornFromElems [r0',n2,n2']
      | x == r2 && y == n2    = sornSingle n2'
      | x == r2 && y == n2'   = sornFromElems [n2', n1, n1']
      | x == r2 && y == n1    = sornSingle nr2
      | x == r2 && y == n1'   = sornSingle nr2'
      | x == r2 && y == nr2   = sornSingle u0
      | x == r2 && y == nr2'  = sornSingle u0'
      | x == r2 && y == u0'   = sornSingle r2'
      | x == r2 && y == r2    = sornSingle u1
      | x == r2 && y == r2'   = sornSingle u1'
      | x == r2 && y == u1    = sornSingle u1'
      | x == r2 && y == u1'   = sornFromElems [u1', u2, u2']
      | x == r2 && y == u2    = sornSingle u2'
      | x == r2 && y == u2'   = sornSingle u2'
      -- /2..
      | x == r2' && y == r0'   = sornFromElems [r0',n2,n2']
      | x == r2' && y == n2    = sornSingle n2'
      | x == r2' && y == n2'   = sornFromElems [n2', n1, n1', nr2, nr2']
      | x == r2' && y == n1    = sornSingle nr2'
      | x == r2' && y == n1'   = sornFromElems [nr2', u0, u0']
      | x == r2' && y == nr2   = sornSingle u0'
      | x == r2' && y == nr2'  = sornFromElems [u0', r2, r2']
      | x == r2' && y == u0'   = sornFromElems [r2', u1, u1']
      | x == r2' && y == r2    = sornSingle u1'
      | x == r2' && y == r2'   = sornSingle u1'
      | x == r2' && y == u1    = sornSingle u1'
      | x == r2' && y == u1'   = sornFromElems [u1', u2, u2']
      | x == r2' && y == u2    = sornSingle u2'
      | x == r2' && y == u2'   = sornSingle u2'
      -- 1
      | x == u1 && y == r0'   = sornFromElems [r0',n2,n2']
      | x == u1 && y == n2    = sornSingle n1
      | x == u1 && y == n2'   = sornFromElems [n1',nr2,nr2']
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
      | x == u1' && y == r0'   = sornFromElems [r0',n2,n2',n1,n1']
      | x == u1' && y == n2    = sornFromElems [n1',nr2,nr2']
      | x == u1' && y == n2'   = sornFromElems [n1',nr2,nr2',u0,r2,r2']
      | x == u1' && y == n1    = sornFromElems [u0',r2,r2']
      | x == u1' && y == n1'   = sornFromElems [u0',r2,r2',u1,u1']
      | x == u1' && y == nr2   = sornFromElems [r2',u1,u1']
      | x == u1' && y == nr2'  = sornFromElems [r2',u1,u1'] 
      | x == u1' && y == u0'   = sornFromElems [u1',u2,u2']
      | x == u1' && y == r2    = sornFromElems [u1',u2,u2'] 
      | x == u1' && y == r2'   = sornFromElems [u1',u2,u2']
      | x == u1' && y == u1    = sornSingle u2'
      | x == u1' && y == u1'   = sornSingle u2'
      | x == u1' && y == u2    = sornSingle u2'
      | x == u1' && y == u2'   = sornSingle u2'
      -- 2
      | x == u2 && y == r0'   = sornFromElems [r0',n2,n2',n1,n1',nr2,nr2']
      | x == u2 && y == n2    = sornSingle u0
      | x == u2 && y == n2'   = sornFromElems [u0',r2,r2']
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
      | x == u2' && y == n2    = sornFromElems [u0',r2,r2',u1,u1',u2,u2']
      | x == u2' && y == n2'   = sornFromElems [u0',r2,r2',u1,u1',u2,u2']
      | x == u2' && y == n1    = sornFromElems [u1',u2,u2']
      | x == u2' && y == n1'   = sornFromElems [u1',u2,u2']
      | x == u2' && y == nr2   = sornFromElems [u1',u2,u2']
      | x == u2' && y == nr2'  = sornFromElems [u1',u2,u2']
      | x == u2' && y == u0'   = sornSingle u2'
      | x == u2' && y == r2    = sornSingle u2'
      | x == u2' && y == r2'   = sornSingle u2'
      | x == u2' && y == u1    = sornSingle u2'
      | x == u2' && y == u1'   = sornSingle u2'
      | x == u2' && y == u2    = sornSingle u2'
      | x == u2' && y == u2'   = sornSingle u2'

      -- "add" commutes
      | otherwise              = sornAddU y x
