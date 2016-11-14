{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tuple helpers
module ViperVM.Utils.Tuple
   ( uncurry4
   , take4
   , fromTuple4
   , module Data.Tuple
   , Single (..)
   , TupleToList
   , ListToTuple
   , ExtractTuple (..)
   )
where

import Data.Tuple
import ViperVM.Utils.Types

-- | Uncurry specialised for quadruple
uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
{-# INLINE uncurry4 #-}
uncurry4 f (a,b,c,d) = f a b c d


-- | Take specialised for quadruple
take4 :: [a] -> (a,a,a,a)
{-# INLINE take4 #-}
take4 [a,b,c,d] = (a,b,c,d)
take4 _         = error "take4: invalid list (exactly 4 elements required)"


-- | toList for quadruple
fromTuple4 :: (a,a,a,a) -> [a]
{-# INLINE fromTuple4 #-}
fromTuple4 (a,b,c,d) = [a,b,c,d]

-- | Singleton type
newtype Single a = Single a 


type family TupleToList t where
   TupleToList (Single a)                                            = '[a]
   TupleToList (a,b)                                                 = '[a,b]
   TupleToList (a,b,c)                                               = '[a,b,c]
   TupleToList (a,b,c,d)                                             = '[a,b,c,d]
   TupleToList (a,b,c,d,e)                                           = '[a,b,c,d,e]
   TupleToList (a,b,c,d,e,f)                                         = '[a,b,c,d,e,f]
   TupleToList (a,b,c,d,e,f,g)                                       = '[a,b,c,d,e,f,g]
   TupleToList (a,b,c,d,e,f,g,h)                                     = '[a,b,c,d,e,f,g,h]
   TupleToList (a,b,c,d,e,f,g,h,i)                                   = '[a,b,c,d,e,f,g,h,i]
   TupleToList (a,b,c,d,e,f,g,h,i,j)                                 = '[a,b,c,d,e,f,g,h,i,j]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k)                               = '[a,b,c,d,e,f,g,h,i,j,k]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l)                             = '[a,b,c,d,e,f,g,h,i,j,k,l]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m)                           = '[a,b,c,d,e,f,g,h,i,j,k,l,m]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n)                         = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)                       = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)                     = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)                   = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)                 = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)               = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)             = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)           = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)         = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)       = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)     = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)   = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y]
   TupleToList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) = '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]

type family ListToTuple t where
   ListToTuple '[a]                                                   = Single a
   ListToTuple '[a,b]                                                 = (a,b)
   ListToTuple '[a,b,c]                                               = (a,b,c)
   ListToTuple '[a,b,c,d]                                             = (a,b,c,d)
   ListToTuple '[a,b,c,d,e]                                           = (a,b,c,d,e)
   ListToTuple '[a,b,c,d,e,f]                                         = (a,b,c,d,e,f)
   ListToTuple '[a,b,c,d,e,f,g]                                       = (a,b,c,d,e,f,g)
   ListToTuple '[a,b,c,d,e,f,g,h]                                     = (a,b,c,d,e,f,g,h)
   ListToTuple '[a,b,c,d,e,f,g,h,i]                                   = (a,b,c,d,e,f,g,h,i)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j]                                 = (a,b,c,d,e,f,g,h,i,j)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k]                               = (a,b,c,d,e,f,g,h,i,j,k)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l]                             = (a,b,c,d,e,f,g,h,i,j,k,l)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m]                           = (a,b,c,d,e,f,g,h,i,j,k,l,m)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n]                         = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]                       = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]                     = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q]                   = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r]                 = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s]               = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t]             = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u]           = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v]         = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w]       = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x]     = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y]   = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)
   ListToTuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)

-- | Extract a tuple value statically
class ExtractTuple (n :: Nat) t x | n t -> x where
   -- | Extract a tuple value by type-level index
   tupleN :: t -> x

instance ExtractTuple 0 (Single t) t where
   {-# INLINE tupleN #-}
   tupleN (Single t) = t

instance ExtractTuple 0 (e0, e1) e0 where
   {-# INLINE tupleN #-}
   tupleN (t,_) = t

instance ExtractTuple 1 (e0, e1) e1 where
   {-# INLINE tupleN #-}
   tupleN (_,t) = t

instance ExtractTuple 0 (e0, e1, e2) e0 where
   {-# INLINE tupleN #-}
   tupleN (t,_,_) = t

instance ExtractTuple 1 (e0, e1, e2) e1 where
   {-# INLINE tupleN #-}
   tupleN (_,t,_) = t

instance ExtractTuple 2 (e0, e1, e2) e2 where
   {-# INLINE tupleN #-}
   tupleN (_,_,t) = t

instance ExtractTuple 0 (e0, e1, e2, e3) e0 where
   {-# INLINE tupleN #-}
   tupleN (t,_,_,_) = t

instance ExtractTuple 1 (e0, e1, e2, e3) e1 where
   {-# INLINE tupleN #-}
   tupleN (_,t,_,_) = t

instance ExtractTuple 2 (e0, e1, e2, e3) e2 where
   {-# INLINE tupleN #-}
   tupleN (_,_,t,_) = t

instance ExtractTuple 3 (e0, e1, e2, e3) e3 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,t) = t


instance ExtractTuple 0 (e0, e1, e2, e3, e4) e0 where
   {-# INLINE tupleN #-}
   tupleN (t,_,_,_,_) = t

instance ExtractTuple 1 (e0, e1, e2, e3, e4) e1 where
   {-# INLINE tupleN #-}
   tupleN (_,t,_,_,_) = t

instance ExtractTuple 2 (e0, e1, e2, e3, e4) e2 where
   {-# INLINE tupleN #-}
   tupleN (_,_,t,_,_) = t

instance ExtractTuple 3 (e0, e1, e2, e3, e4) e3 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,t,_) = t

instance ExtractTuple 4 (e0, e1, e2, e3, e4) e4 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,t) = t


instance ExtractTuple 0 (e0, e1, e2, e3, e4, e5) e0 where
   {-# INLINE tupleN #-}
   tupleN (t,_,_,_,_,_) = t

instance ExtractTuple 1 (e0, e1, e2, e3, e4, e5) e1 where
   {-# INLINE tupleN #-}
   tupleN (_,t,_,_,_,_) = t

instance ExtractTuple 2 (e0, e1, e2, e3, e4, e5) e2 where
   {-# INLINE tupleN #-}
   tupleN (_,_,t,_,_,_) = t

instance ExtractTuple 3 (e0, e1, e2, e3, e4, e5) e3 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,t,_,_) = t

instance ExtractTuple 4 (e0, e1, e2, e3, e4, e5) e4 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,t,_) = t

instance ExtractTuple 5 (e0, e1, e2, e3, e4, e5) e5 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,_,t) = t


instance ExtractTuple 0 (e0, e1, e2, e3, e4, e5, e6) e0 where
   {-# INLINE tupleN #-}
   tupleN (t,_,_,_,_,_,_) = t

instance ExtractTuple 1 (e0, e1, e2, e3, e4, e5, e6) e1 where
   {-# INLINE tupleN #-}
   tupleN (_,t,_,_,_,_,_) = t

instance ExtractTuple 2 (e0, e1, e2, e3, e4, e5, e6) e2 where
   {-# INLINE tupleN #-}
   tupleN (_,_,t,_,_,_,_) = t

instance ExtractTuple 3 (e0, e1, e2, e3, e4, e5, e6) e3 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,t,_,_,_) = t

instance ExtractTuple 4 (e0, e1, e2, e3, e4, e5, e6) e4 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,t,_,_) = t

instance ExtractTuple 5 (e0, e1, e2, e3, e4, e5, e6) e5 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,_,t,_) = t

instance ExtractTuple 6 (e0, e1, e2, e3, e4, e5, e6) e6 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,_,_,t) = t


instance ExtractTuple 0 (e0, e1, e2, e3, e4, e5, e6, e7) e0 where
   {-# INLINE tupleN #-}
   tupleN (t,_,_,_,_,_,_,_) = t

instance ExtractTuple 1 (e0, e1, e2, e3, e4, e5, e6, e7) e1 where
   {-# INLINE tupleN #-}
   tupleN (_,t,_,_,_,_,_,_) = t

instance ExtractTuple 2 (e0, e1, e2, e3, e4, e5, e6, e7) e2 where
   {-# INLINE tupleN #-}
   tupleN (_,_,t,_,_,_,_,_) = t

instance ExtractTuple 3 (e0, e1, e2, e3, e4, e5, e6, e7) e3 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,t,_,_,_,_) = t

instance ExtractTuple 4 (e0, e1, e2, e3, e4, e5, e6, e7) e4 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,t,_,_,_) = t

instance ExtractTuple 5 (e0, e1, e2, e3, e4, e5, e6, e7) e5 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,_,t,_,_) = t

instance ExtractTuple 6 (e0, e1, e2, e3, e4, e5, e6, e7) e6 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,_,_,t,_) = t

instance ExtractTuple 7 (e0, e1, e2, e3, e4, e5, e6, e7) e7 where
   {-# INLINE tupleN #-}
   tupleN (_,_,_,_,_,_,_,t) = t
