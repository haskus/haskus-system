{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViperVM.Utils.Parser
   ( LexicalError (..)
   , SemanticError (..)
   , EndOfInputError (..)
   , Choice (..)
   , choice
   , choice'
   )
where

import ViperVM.Utils.HList
import ViperVM.Utils.Flow
import Data.Proxy


-- A parser is a function that can either:
--    - return a parsed value or operate on already parsed values
--    - fail: we distinguish several kinds of failures
--       - not enough input
--       - lexical error: cannot read what it is supposed to from the input
--       - semantic error: what is read isn't coherent with what has been
--       previously read
--


data LexicalError    = LexicalError
data SemanticError a = SemanticError a
data EndOfInputError = EndOfInputError

-- We can define combinators between parsers

data Choice a = Choice

instance forall x y z xs ys zs m a.
      ( x ~ Flow m xs
      , y ~ Flow m ys
      , z ~ Flow m zs
      , Catchable a xs
      , Liftable ys zs
      , Liftable (Filter a xs) zs
      , zs ~ Fusion (Filter a xs) ys
      , Monad m
      ) => ApplyAB (Choice a) (x,y) z
   where
      applyAB _ (x,y) = x >%~&> \(_ :: a) -> y

-- | Try to apply the actions in the list in order, until one of them succeeds.
-- Returns the value of the succeeding action, or the value of the last one.
-- Failures are detected with values of type "LexicalError".
choice :: forall m fs zs.
   ( Monad m
   , HFoldl (Choice LexicalError) (Flow m '[LexicalError]) fs (Flow m zs)
   ) => HList fs -> Flow m zs
choice = choice' (Proxy :: Proxy LexicalError)

-- | Try to apply the actions in the list in order, until one of them succeeds.
-- Returns the value of the succeeding action, or the value of the last one.
-- Failures are detected with values of type "a".
choice' :: forall m fs zs a.
   ( Monad m
   , HFoldl (Choice a) (Flow m '[a]) fs (Flow m zs)
   ) => Proxy a -> HList fs -> Flow m zs
choice' _ = hFoldl (Choice :: Choice a) (flowRet' undefined :: Flow m '[a])
