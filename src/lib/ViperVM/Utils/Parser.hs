{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tools to write parsers using Flows
module ViperVM.Utils.Parser
   ( ParseError (..)
   , Choice (..)
   , choice
   , choice'
   , manyBounded
   , manyAtMost
   , manyAtMost'
   , manyAtMost''
   , many
   , manyAtLeast
   , manyTill
   , manyTill'
   )
where

import Prelude hiding (min,max)
import ViperVM.Utils.HList
import ViperVM.Utils.Types
import ViperVM.Utils.Types.List
import ViperVM.Utils.Flow
import ViperVM.Utils.Variant


-- A parser is a Flow function that can either:
--    - return a parsed value or a semantic error
--    - fail with a ParseError:
--       - not enough input
--       - syntax error
--

-- | Parser error
data ParseError
   = SyntaxError
   | EndOfInput
   deriving (Show,Eq)

-- We can define combinators between parsers

data Choice a = Choice

instance forall x y z xs ys zs m a.
      ( x ~ Flow m xs
      , y ~ Flow m ys
      , z ~ Flow m zs
      , Catchable a xs
      , Liftable ys zs
      , Liftable (Filter a xs) zs
      , zs ~ Union (Filter a xs) ys
      , Monad m
      ) => Apply (Choice a) (x,y) z
   where
      apply _ (x,y) = x >%~|> \(_ :: a) -> y

-- | Try to apply the actions in the list in order, until one of them succeeds.
-- Returns the value of the succeeding action, or the value of the last one.
-- Failures are detected with values of type "ParseError".
choice :: forall m fs zs.
   ( Monad m
   , HFoldl (Choice ParseError) (Flow m '[ParseError]) fs (Flow m zs)
   ) => HList fs -> Flow m zs
choice = choice' (Proxy :: Proxy ParseError)

-- | Try to apply the actions in the list in order, until one of them succeeds.
-- Returns the value of the succeeding action, or the value of the last one.
-- Failures are detected with values of type "a".
choice' :: forall m fs zs a.
   ( Monad m
   , HFoldl (Choice a) (Flow m '[a]) fs (Flow m zs)
   ) => Proxy a -> HList fs -> Flow m zs
choice' _ = hFoldl (Choice :: Choice a) (flowRet0' undefined :: Flow m '[a])

-- | Apply the action zero or more times (until a ParseError result is
-- returned)
many ::
   ( zs ~ Filter ParseError xs
   , Monad m
   , Catchable ParseError xs
   ) => Flow m xs -> Flow m '[[Variant zs]]
many f = manyBounded Nothing Nothing f
            >%~^> \(_ :: ParseError) -> flowRet0' []

-- | Apply the action zero or more times (up to max) until a ParseError result
-- is returned
manyAtMost ::
   ( zs ~ Filter ParseError xs
   , Monad m
   , Catchable ParseError xs
   ) => Word -> Flow m xs -> Flow m '[[Variant zs]]
manyAtMost max f = manyBounded Nothing (Just max) f
                     >%~^> \(_ :: ParseError) -> flowRet0' []

-- | Apply the action zero or more times (up to max) until a ParseError result
-- is returned
manyAtMost' ::
   ( zs ~ Filter ParseError xs
   , Monad m
   , Catchable ParseError xs
   ) => Word -> Flow m xs -> m [Variant zs]
manyAtMost' max f = singleVariant <$> manyAtMost max f

-- | Apply the action zero or more times (up to max) until a ParseError result
-- is returned
manyAtMost'' ::
   ( '[x] ~ Filter ParseError xs
   , Monad m
   , Catchable ParseError xs
   ) => Word -> Flow m xs -> m [x]
manyAtMost'' max f = fmap singleVariant <$> manyAtMost' max f

-- | Apply the action at least n times or more times (until a ParseError
-- result is returned)
manyAtLeast ::
   ( zs ~ Filter ParseError xs
   , Monad m
   , Catchable ParseError xs
   ) => Word -> Flow m xs -> Flow m '[[Variant zs],ParseError]
manyAtLeast min = manyBounded (Just min) Nothing

-- | Apply the first action zero or more times until the second succeeds.
-- If the first action fails, the whole operation fails.
--
-- Return both the list of first values and the ending value
manyTill ::
   ( zs ~ Filter ParseError xs
   , zs' ~ Filter ParseError ys
   , Monad m
   , MaybeCatchable ParseError xs
   , Catchable ParseError ys
   ) => Flow m xs -> Flow m ys -> Flow m '[([Variant zs],Variant zs'),ParseError]
manyTill f g = go []
   where
      go xs = do
         v <- g
         case catchVariant v of
            Right EndOfInput  -> flowSet EndOfInput
            Right SyntaxError -> do
               u <- f
               case catchVariant u of
                  Right (e :: ParseError) -> flowSet e
                  Left x                  -> go (x:xs)
            Left x            -> flowSet (reverse xs,x)

-- | Apply the first action zero or more times until the second succeeds.
-- If the first action fails, the whole operation fails.
--
-- Return only the list of first values
manyTill' ::
   ( zs ~ Filter ParseError xs
   , Monad m
   , MaybeCatchable ParseError xs
   , Catchable ParseError ys
   ) => Flow m xs -> Flow m ys -> Flow m '[[Variant zs],ParseError]
manyTill' f g = manyTill f g >.-.> fst

-- | Apply the given action at least 'min' times and at most 'max' time
--
-- On failure, fails.
manyBounded :: forall zs xs m.
   ( zs ~ Filter ParseError xs
   , Monad m
   , MaybeCatchable ParseError xs
   ) => Maybe Word -> Maybe Word -> Flow m xs -> Flow m '[[Variant zs],ParseError]
manyBounded _ (Just 0) _   = flowSet ([] :: [Variant zs])
manyBounded (Just 0) max f = manyBounded Nothing max f
manyBounded min max f      = do
   v <- f
   case catchVariant v of
      Right (e :: ParseError) -> case min of
         Just n | n > 0 -> flowSet e
         _              -> flowSet ([] :: [Variant zs])
      Left x           -> do
         let minus1 = fmap (\k -> k - 1)
         xs <- manyBounded (minus1 min) (minus1 max) f
         case toEither xs of
            Left (e :: ParseError) -> flowSet e
            Right xs'              -> flowSet (x : xs')

