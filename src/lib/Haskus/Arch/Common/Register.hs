{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Register
module Haskus.Arch.Common.Register
   ( Reg (..)
   , TypedReg (..)
   -- * Register family
   , RegFam (..)
   , Qualifier
   , CSet (..)
   , regMatchFamily
   , regFixupFamily
   , regFixupFamilyMaybe
   , matchQualifier
   , reduceQualifier
   , trySetQualifier
   , regFamFromReg
   )
where

import Haskus.Utils.Solver
import Haskus.Utils.List (nub)

-- | Register
data Reg banks = Reg
   { registerBank   :: banks                -- ^ Register bank
   , registerId     :: {-# UNPACK #-} !Word -- ^ Register ID
   , registerSize   :: {-# UNPACK #-} !Word -- ^ Register size in bits
   , registerOffset :: {-# UNPACK #-} !Word -- ^ Register offset in bits (alias register)
   }
   deriving (Show,Eq,Ord)


-- | Typed register
data TypedReg typ bank = TypedReg
   { typedReg     :: Reg bank   -- ^ Register
   , typedRegType :: typ        -- ^ Register type
   }
   deriving (Show,Eq,Ord)

--------------------------
-- Register family
-- ~~~~~~~~~~~~~~~
--
-- A register family is a set of registers that have some things in common.
-- We encode them similarly to usual Register except that their properties
-- support qualifiers (OneOf,NoneOf...).
--
-- We also have a `Guard p a` qualifier which uses `p` as a guard for `a`:
-- during matching, a function to evalute `p` must be provided. It is used for
-- instance for x86 register families which depend on the operand size: we guard
-- each register variant (AX,EAX,RAX) with a predicate `OperandSizeEq n` (with
-- n=16,32,64 respectively).
--


-- | Constrained Set
data CSet a
   = Singleton a     -- ^ {a}
   | NoneOf [a]      -- ^ {*} \ {a,...}
   | OneOf  [a]      -- ^ {a,...}
   | Any             -- ^ {*}
   | None            -- ^ {}
   deriving (Show,Eq)

-- | Test if an element is in a set
elemSet :: Eq a => a -> CSet a -> Bool
elemSet y (Singleton x) = x == y
elemSet y (NoneOf xs)   = y `notElem` xs
elemSet y (OneOf xs)    = y `elem` xs
elemSet _ Any           = True
elemSet _ None          = False

-- | Simplify a set if possible
simplifySet :: CSet a -> CSet a
simplifySet s = case s of
   OneOf  [x]  -> Singleton x
   NoneOf []   -> Any
   OneOf  _    -> s
   NoneOf _    -> s
   Singleton _ -> s
   Any         -> s
   None        -> s

type Qualifier p a = Rule () p (CSet a)

-- | Register family
data RegFam pred banks = RegFam
   { regFamBank   :: Qualifier pred banks -- ^ Register bank
   , regFamId     :: Qualifier pred Word  -- ^ Register ID
   , regFamSize   :: Qualifier pred Word  -- ^ Register size in bits
   , regFamOffset :: Qualifier pred Word  -- ^ Register offset in bits
   }
   deriving (Show,Eq)

instance (Eq b, Eq p) => Predicated (RegFam p b) where
   type Pred (RegFam p b)    = p
   type PredErr (RegFam p b) = ()
   reducePredicates fp (RegFam b i s o) =
      RegFam <$> reducePredicates fp b
             <*> reducePredicates fp i
             <*> reducePredicates fp s
             <*> reducePredicates fp o

   getTerminals (RegFam bs is ss os) = [ RegFam b i s o | b <- getTerminals bs
                                                        , i <- getTerminals is
                                                        , s <- getTerminals ss
                                                        , o <- getTerminals os
                                       ]
   getPredicates (RegFam b i s o) = nub $ concat [ getPredicates b
                                                 , getPredicates i
                                                 , getPredicates s
                                                 , getPredicates o
                                                 ]


-- | Test if a register match a family
regMatchFamily :: (Eq p, Eq b) => (p -> Maybe Bool) -> RegFam p b -> Reg b -> Bool
regMatchFamily predSolver RegFam{..} Reg{..} =
      matchQualifier predSolver registerBank regFamBank
      && matchQualifier predSolver registerId regFamId
      && matchQualifier predSolver registerSize regFamSize
      && matchQualifier predSolver registerOffset regFamOffset

-- | Fixup a family (all fields must reduce to Set qualifier)
regFixupFamily :: (Eq p,Eq b,Show b,Show p) => (p -> Maybe Bool) -> RegFam p b -> Reg b
regFixupFamily predSolver fam =
   case regFixupFamilyMaybe predSolver fam of
      Nothing -> error ("Cannot fixup family: " ++ show fam)
      Just c  -> c

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFixupFamilyMaybe ::
   ( Predicated (RegFam p b)
   , Eq b
   ) => (p -> Maybe Bool) -> RegFam p b -> Maybe (Reg b)
regFixupFamilyMaybe fp rf =
   case reducePredicates fp rf of
      Match (RegFam (Terminal a) (Terminal b) (Terminal c) (Terminal d)) ->
         case (simplifySet a, simplifySet b, simplifySet c, simplifySet d) of
            (Singleton u, Singleton v, Singleton w, Singleton x) -> Just (Reg u v w x)
            _                                                    -> Nothing
      _ -> Nothing

-- | Reduce a qualifier as much as possible
reduceQualifier :: (Eq p, Eq a) => (p -> Maybe Bool) -> Qualifier p a -> Qualifier p a
reduceQualifier fp x = case ruleReduce fp x of
      NoMatch          -> Terminal None
      DivergentMatch _ -> error "reduceQualifier: divergent qualifier"
      MatchFail _      -> Terminal None
      Match a          -> Terminal (simplifySet a)
      MatchRule r      -> r

-- | Match a qualifier
matchQualifier :: (Eq p, Eq a) => (p -> Maybe Bool) -> a -> Qualifier p a -> Bool
matchQualifier fp y q = case ruleReduce fp q of
   NoMatch          -> False
   DivergentMatch _ -> False
   MatchFail _      -> False
   Match s          -> y `elemSet` s
   MatchRule _      -> False

-- | Try to set the value of a qualifier if it matches. Otherwise leave it
-- untouched
trySetQualifier :: (Eq p, Eq a) => (p -> Maybe Bool) -> a -> Qualifier p a -> Qualifier p a
trySetQualifier oracle a q = fmap f q
   where
      f | matchQualifier oracle a q = \_ -> Singleton a
        | otherwise                 = id


-- | Make a family matching a single register
regFamFromReg :: Reg b -> RegFam p b
regFamFromReg Reg{..} = RegFam
   { regFamBank   = Terminal $ Singleton registerBank
   , regFamId     = Terminal $ Singleton registerId
   , regFamSize   = Terminal $ Singleton registerSize
   , regFamOffset = Terminal $ Singleton registerOffset
   }
