{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | A binary reader monad
--
-- Rely on the MultiState monad to allow concurrent Readers/Writers.
module ViperVM.Format.Binary.Reader
   ( Reader (..)
   , ReaderM
   , binReadWithT
   , binReadWith
   , binReadWithT'
   , binReadWith'
   , binReadWithIfT
   , binReadWithIf
   , binReadIfT
   , binReadIf
   , binReadIfT'
   , binReadIf'
   , binReadWithIfT'
   , binReadWithIf'
   , binLookAheadT
   , binLookAhead
   , binLookAheadTestT
   , binLookAheadTest
   , binReadTupleT
   , binReadTuple
   -- * TODO: cleanup
   , binReadT
   , binRead
   , binPeekT
   , binPeek
   , binTryReadT
   , binTryRead
   , binTryReadIfT
   , binTryReadIf
   , binTryPeekT
   , binTryPeek
   , binSkipT
   , binSkip
   , binRemainingT
   , binRemaining
   , binWithIfT
   , binWithIf
   , binWithT
   , binWith
   )
where

import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Word
import ViperVM.Utils.MultiState
import ViperVM.Utils.HArray
import ViperVM.Utils.HList
import ViperVM.Utils.Parser
import ViperVM.Utils.Variant
import ViperVM.Utils.Flow

import Data.Proxy
import Data.Maybe (isJust)
import Foreign.Storable

-- | The phantom type `a` is used to distinguish between several readers
newtype Reader a = Reader Buffer

-- | Constraint: reader 'r' is in state 's'
type ReaderM r s =
   ( HArrayIndexT (Reader r) s)

-- | Read an element from the specified reader
binReadT :: forall r m s a. (Monad m, ReaderM r s, Storable a) => Proxy r -> MStateT s m a
binReadT _ = do
   Reader b <- mGet :: MStateT s m (Reader r)
   let (b',a) = bufferRead b
   mSet (Reader b' :: Reader r)
   return a

-- | Read an element from a predefined `Reader ()`
binRead :: forall m s a. (Monad m, ReaderM () s, Storable a) => MStateT s m a
binRead = binReadT (Proxy :: Proxy ()) 

-- | Peek an element from the specified reader
binPeekT :: forall r m s a. (Monad m, ReaderM r s, Storable a) => Proxy r -> MStateT s m a
binPeekT _ = do
   Reader b <- mGet :: MStateT s m (Reader r)
   let (_,a) = bufferRead b
   return a

-- | Peek an element from a predefined `Reader ()`
binPeek :: forall m s a. (Monad m, ReaderM () s, Storable a) => MStateT s m a
binPeek = binPeekT (Proxy :: Proxy ()) 


-- | Skip the specified number of bytes
binSkipT :: forall r m s. (Monad m, ReaderM r s) => Proxy r -> Word64 -> MStateT s m ()
binSkipT _ n = do
   Reader b <- mGet :: MStateT s m (Reader r)
   let b' = bufferDrop n b
   mSet (Reader b' :: Reader r)

-- | Skip the specified number of bytes
binSkip :: forall m s. (Monad m, ReaderM () s) => Word64 -> MStateT s m ()
binSkip = binSkipT (Proxy :: Proxy ())

-- | Get the remaining number of bytes
binRemainingT :: forall r m s. (Monad m, ReaderM r s) => Proxy r -> MStateT s m Word64
binRemainingT _ = do
   Reader b <- mGet :: MStateT s m (Reader r)
   return (bufferSize b)

-- | Get the remaining number of bytes
binRemaining :: forall m s. (Monad m, ReaderM () s) => MStateT s m Word64
binRemaining = binRemainingT (Proxy :: Proxy ())

-- | Try to read if there are enough bytes
binTryReadT :: forall r m s a.
   ( Monad m
   , ReaderM r s
   , Storable a) => Proxy r -> MStateT s m (Maybe a)
binTryReadT _ = binRemainingT (Proxy :: Proxy r) >>= \sz ->
   if sz >= fromIntegral (sizeOf (undefined :: a))
      then Just <$> binReadT (Proxy :: Proxy r)
      else return Nothing

-- | Try to read if there are enough bytes
binTryRead :: forall m s a.
   ( Monad m
   , ReaderM () s
   , Storable a) => MStateT s m (Maybe a)
binTryRead = binTryReadT (Proxy :: Proxy ())

-- | Try to read if there are enough bytes and if the condition is met
binTryReadIfT :: forall r m s a.
   ( Monad m
   , ReaderM r s
   , Storable a) => Proxy r -> (a -> Bool) -> MStateT s m (Maybe a)
binTryReadIfT r cond = binTryPeekT r >>= \case
   Just w | cond w -> binSkipT r (fromIntegral (sizeOf w)) >> return (Just w)
   _               -> return Nothing

-- | Try to read if there are enough bytes and if the condition is met
binTryReadIf :: forall m s a.
   ( Monad m
   , ReaderM () s
   , Storable a) => (a -> Bool) -> MStateT s m (Maybe a)
binTryReadIf = binTryReadIfT (Proxy :: Proxy ())

-- | Try to peek if there are enough bytes
binTryPeekT :: forall r m s a.
   ( Monad m
   , ReaderM r s
   , Storable a) => Proxy r -> MStateT s m (Maybe a)
binTryPeekT _ = binRemainingT (Proxy :: Proxy r) >>= \sz ->
   if sz >= fromIntegral (sizeOf (undefined :: a))
      then Just <$> binPeekT (Proxy :: Proxy r)
      else return Nothing

-- | Try to peek if there are enough bytes
binTryPeek :: forall m s a.
   ( Monad m
   , ReaderM () s
   , Storable a) => MStateT s m (Maybe a)
binTryPeek = binTryPeekT (Proxy :: Proxy ())

-- | Reader with backtracking
--
-- Warning: only the reader is backtracked. Be careful when you change other
-- parts of the state.
binWithIfT :: forall r m s a b.
   ( Monad m
   , ReaderM r s
   , Storable a) => Proxy r -> (a -> Bool) -> (a -> MStateT s m (Maybe b)) -> MStateT s m (Maybe b)
binWithIfT r cond f = do
   -- save the reader
   rdr <- mGet :: MStateT s m (Reader r)

   -- try to read and apply the function
   res <- binTryReadIfT r cond >>= \case
      Nothing -> return Nothing
      Just a  -> f a

   -- backtrack if necessary
   case res of
      Nothing -> mSet rdr >> return Nothing
      Just _  -> return res

-- | Reader with backtracking
--
-- Warning: only the reader is backtracked. Be careful when you change other
-- parts of the state.
binWithIf :: forall m s a b.
   ( Monad m
   , ReaderM () s
   , Storable a) => (a -> Bool) -> (a -> MStateT s m (Maybe b)) -> MStateT s m (Maybe b)
binWithIf = binWithIfT (Proxy :: Proxy ())

-- | Reader with backtracking
--
-- Warning: only the reader is backtracked. Be careful when you change other
-- parts of the state.
binWithT :: forall r m s a b.
   ( Monad m
   , ReaderM r s
   , Storable a) => Proxy r -> (a -> MStateT s m (Maybe b)) -> MStateT s m (Maybe b)
binWithT r = binWithIfT r (const True)

-- | Reader with backtracking
--
-- Warning: only the reader is backtracked. Be careful when you change other
-- parts of the state.
binWith :: forall m s a b.
   ( Monad m
   , ReaderM () s
   , Storable a) => (a -> MStateT s m (Maybe b)) -> MStateT s m (Maybe b)
binWith = binWithT (Proxy :: Proxy ())


-- | Try to read something
--
-- Backtrack on ParseError
binReadWithT :: forall r m s a xs.
   ( Monad m
   , ReaderM r s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   ) => Proxy r -> (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWithT p f = do
   -- get the reader
   Reader buf <- mGet :: MStateT s m (Reader r)

   -- check the size
   sz <- binRemainingT p
   if sz < fromIntegral (sizeOf (undefined :: a))
      then flowSet EndOfInput
      else do
         let (buf',a) = bufferRead buf
         -- update the state
         mSet (Reader buf' :: Reader r)
         -- execute the function
         f a >%~=> \(_ :: ParseError) ->
               -- backtrack
               mSet (Reader buf :: Reader r)


-- | Try to read something
--
-- Backtrack on ParseError
binReadWith :: forall m s a xs.
   ( Monad m
   , ReaderM () s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   ) => (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWith = binReadWithT (Proxy :: Proxy ())


-- | Try to read something
--
-- Backtrack on ParseError
binReadWithT' :: forall r m s a xs.
   ( Monad m
   , ReaderM r s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , xs ~ '[a,ParseError]
   ) => Proxy r -> (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWithT' = binReadWithT


-- | Try to read something
--
-- Backtrack on ParseError
binReadWith' :: forall m s a xs.
   ( Monad m
   , ReaderM () s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , xs ~ '[a,ParseError]
   ) => (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWith' = binReadWith


-- | Try to read something
--
-- Backtrack on ParseError
binReadWithIfT :: forall r m s a xs.
   ( Monad m
   , ReaderM r s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   ) => Proxy r -> (a -> Bool) -> (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWithIfT p c f =
   binReadWithT p $ \a -> if c a
      then f a
      else flowSet SyntaxError


-- | Try to read something
--
-- Backtrack on ParseError
binReadWithIf :: forall m s a xs.
   ( Monad m
   , ReaderM () s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   ) => (a -> Bool) -> (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWithIf = binReadWithIfT (Proxy :: Proxy ())


-- | Try to read something
--
-- Backtrack on ParseError
binReadWithIfT' :: forall r m s a xs.
   ( Monad m
   , ReaderM r s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , xs ~ '[a,ParseError]
   ) => Proxy r -> (a -> Bool) -> (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWithIfT' p c f =
   binReadWithT p $ \a -> if c a
      then f a
      else flowSet SyntaxError


-- | Try to read something
--
-- Backtrack on ParseError
binReadWithIf' :: forall m s a xs.
   ( Monad m
   , ReaderM () s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , xs ~ '[a,ParseError]
   ) => (a -> Bool) -> (a -> MStateT s m (Variant xs)) -> MStateT s m (Variant xs)
binReadWithIf' = binReadWithIfT (Proxy :: Proxy ())


-- | Try to read something
--
-- Backtrack on ParseError
binReadIfT :: forall r m s a xs.
   ( Monad m
   , ReaderM r s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , Member (Maybe a) xs
   ) => Proxy r -> (a -> Bool) -> MStateT s m (Variant xs)
binReadIfT p c =
   binReadWithT p $ \a -> if c a
      then flowSet (Just a)
      else flowSet (Nothing :: Maybe a)


-- | Try to read something
--
-- Backtrack on ParseError
binReadIf :: forall m s a xs.
   ( Monad m
   , ReaderM () s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , Member (Maybe a) xs
   ) => (a -> Bool) -> MStateT s m (Variant xs)
binReadIf = binReadIfT (Proxy :: Proxy ())

-- | Try to read something
--
-- Backtrack on ParseError
binReadIfT' :: forall r m s a xs.
   ( Monad m
   , ReaderM r s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , Member (Maybe a) xs
   , xs ~ '[Maybe a, ParseError]
   ) => Proxy r -> (a -> Bool) -> MStateT s m (Variant '[Maybe a, ParseError])
binReadIfT' p c =
   binReadWithT p $ \a -> if c a
      then flowSet (Just a)
      else flowSet (Nothing :: Maybe a)


-- | Try to read something
--
-- Backtrack on ParseError
binReadIf' :: forall m s a xs.
   ( Monad m
   , ReaderM () s
   , Storable a
   , Catchable ParseError xs
   , Member ParseError xs
   , Member (Maybe a) xs
   , xs ~ '[Maybe a, ParseError]
   ) => (a -> Bool) -> MStateT s m (Variant '[Maybe a, ParseError])
binReadIf' = binReadIfT (Proxy :: Proxy ())

-- | Look-ahead. Only backtrack the reader, not the other parts of the state
binLookAheadT :: forall r m s xs.
   ( Monad m
   , ReaderM r s
   ) => Proxy r -> MStateT s m (Variant xs) -> MStateT s m (Variant xs)
binLookAheadT _ f = do
   -- get the reader
   Reader buf <- mGet :: MStateT s m (Reader r)

   -- execute the function
   r <- f

   -- backtrack
   mSet (Reader buf :: Reader r)

   return r

-- | Look-ahead. Only backtrack the reader, not the other parts of the state
binLookAhead :: forall m s xs.
   ( Monad m
   , ReaderM () s
   ) => MStateT s m (Variant xs) -> MStateT s m (Variant xs)
binLookAhead = binLookAheadT (Proxy :: Proxy ())


-- | Look-ahead test. Only backtrack the reader, not the other parts of the state
binLookAheadTestT :: forall r m s a.
   ( Monad m
   , ReaderM r s
   , Storable a
   ) => Proxy r -> (a -> Bool) -> MStateT s m Bool
binLookAheadTestT p c = singleVariant <$> f
   where
      f = binLookAheadT p (binReadIfT' p c) 
            >.-.> isJust
            >%~#> \(_ :: ParseError) -> flowRet False

-- | Look-ahead. Only backtrack the reader, not the other parts of the state
binLookAheadTest :: forall m s a.
   ( Monad m
   , ReaderM () s
   , Storable a
   ) => (a -> Bool) -> MStateT s m Bool
binLookAheadTest = binLookAheadTestT (Proxy :: Proxy ())

data ReadTuple r = ReadTuple

instance forall z x z' m s r.
   ( z' ~ MStateT s m (Variant '[HList (x ': z),ParseError])
   , ReaderM r s
   , Monad m
   , Storable x
   ) => ApplyAB (ReadTuple r) (HList z,x) z' where
      applyAB _ (z,_) = binReadWithT (Proxy :: Proxy r) $ \(x :: x) ->
         flowRet (x `HCons` z) :: z'

-- | Read a tuple
binReadTupleT :: forall v (v2 :: [*]) t r s m z.
   ( HTuple' v t
   , HFoldl' (ReadTuple r) (HList '[]) v z
   , z ~  MStateT s m (Variant '[HList v2,ParseError])
   , Monad m
   , HReverse v2 v
   , ReaderM r s
   ) => Proxy r -> MStateT s m (Variant '[t,ParseError])
binReadTupleT _ = do
   let v = hFromTuple' (undefined :: t) :: HList v
   (hFoldl' (ReadTuple :: ReadTuple r) (HNil :: HList '[]) v :: z)
      >.-.> \(v' :: HList v2) -> hToTuple' (hReverse v' :: HList v)


-- | Read a tuple
binReadTuple :: forall v v2 t s m z.
   ( HTuple' v t
   , HFoldl' (ReadTuple ()) (HList '[]) v z
   , z ~  MStateT s m (Variant '[HList v2,ParseError])
   , Monad m
   , HReverse v2 v
   , ReaderM () s
   ) => MStateT s m (Variant '[t,ParseError])
binReadTuple = binReadTupleT (Proxy :: Proxy ())
