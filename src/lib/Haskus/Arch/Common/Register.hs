{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Register
module Haskus.Arch.Common.Register
   ( Reg (..)
   , TypedReg (..)
   -- * Register family
   , RegFam (..)
   , PredRegFam
   , TermRegFam
   , regMatchFamily
   , regFixupPredFamily
   , regFixupPredFamilyMaybe
   , regFixupTermFamilyMaybe
   , regFixupTermFamily
   , regFamFromReg
   , pRegFamFromReg
   -- * Set
   , CSet (..)
   , PSet
   , matchPSet
   , reducePSet
   , trySetPSet
   , trySetCSet
   )
where

import Haskus.Utils.Solver
import Haskus.Utils.Flow
import Haskus.Utils.Maybe
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
   deriving (Show,Eq,Ord)

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

type PSet e p a = Rule e p (CSet a)

data T      -- terminal
data NT p e -- non-terminal

type family Q t a :: * where
   Q (NT p e) a = Rule e p a
   Q T        a = a

-- | Register family
data RegFam t banks = RegFam
   { regFamBank   :: !(Q t (CSet banks)) -- ^ Register bank
   , regFamId     :: !(Q t (CSet Word))  -- ^ Register ID
   , regFamSize   :: !(Q t (CSet Word))  -- ^ Register size in bits
   , regFamOffset :: !(Q t (CSet Word))  -- ^ Register offset in bits
   }

type PredRegFam p e b = RegFam (NT p e) b
type TermRegFam b     = RegFam T b

deriving instance Show b => Show (RegFam T b)
deriving instance Eq b   => Eq   (RegFam T b)
deriving instance Ord b  => Ord  (RegFam T b)
deriving instance (Show p, Show e, Show b) => Show (RegFam (NT p e) b)
deriving instance (Eq p, Eq e, Eq b)       => Eq   (RegFam (NT p e) b)
deriving instance (Ord p, Ord e, Ord b)    => Ord  (RegFam (NT p e) b)

instance (Ord p, Eq e, Eq b, Eq p) => Predicated (RegFam (NT p e) b) where
   type Pred     (RegFam (NT p e) b) = p
   type PredErr  (RegFam (NT p e) b) = e
   type PredTerm (RegFam (NT p e) b) = RegFam T b

   liftTerminal (RegFam b i s o) = RegFam (liftTerminal b)
                                          (liftTerminal i)
                                          (liftTerminal s)
                                          (liftTerminal o)

   reducePredicates oracle (RegFam b i s o) =
      initP RegFam RegFam
         |> (`applyP` reducePredicates oracle b)
         |> (`applyP` reducePredicates oracle i)
         |> (`applyP` reducePredicates oracle s)
         |> (`applyP` reducePredicates oracle o)
         |> resultP

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
regMatchFamily ::
   ( Ord p
   , Eq b
   , Predicated (PredRegFam p e b)
   ) => PredOracle p -> PredRegFam p e b -> Reg b -> Bool
regMatchFamily oracle rf Reg{..} =
   case reducePredicates oracle rf of
      Match (RegFam {..}) -> registerBank `elemSet` regFamBank
                              && registerId `elemSet` regFamId
                              && registerSize `elemSet` regFamSize
                              && registerOffset `elemSet` regFamOffset
      _ -> False

-- | Fixup a family (all fields must reduce to Singleton set)
regFixupPredFamily ::
   ( Predicated (PredRegFam p e b)
   , Show (PredRegFam p e b)
   , Eq b
   , Ord p
   ) => PredOracle p -> PredRegFam p e b -> Reg b
regFixupPredFamily oracle fam =
   case regFixupPredFamilyMaybe oracle fam of
      Nothing -> error ("Cannot fixup family: " ++ show fam)
      Just c  -> c

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFixupPredFamilyMaybe ::
   ( Predicated (PredRegFam p e b)
   , Eq b
   , Ord p
   ) => PredOracle p -> PredRegFam p e b -> Maybe (Reg b)
regFixupPredFamilyMaybe oracle fam =
   case reducePredicates oracle fam of
      Match r -> regFixupTermFamilyMaybe r
      _       -> Nothing

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFixupTermFamilyMaybe :: Eq b => TermRegFam b -> Maybe (Reg b)
regFixupTermFamilyMaybe (RegFam a b c d) =
   case (simplifySet a, simplifySet b, simplifySet c, simplifySet d) of
      (Singleton u, Singleton v, Singleton w, Singleton x) -> Just (Reg u v w x)
      _                                                    -> Nothing

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFixupTermFamily :: (Show (TermRegFam b), Eq b) => TermRegFam b -> Reg b
regFixupTermFamily fam = fromMaybe err (regFixupTermFamilyMaybe fam)
   where
      err = error ("Cannot fixup family: " ++ show fam)

-- | Reduce a qualifier as much as possible
reducePSet :: (Eq e, Eq p, Eq a, Ord p) => PredOracle p -> PSet e p a -> PSet e p a
reducePSet oracle x = case reducePredicates oracle x of
      NoMatch          -> liftTerminal None
      MatchDiverge _   -> liftTerminal None
      MatchFail _      -> liftTerminal None
      Match a          -> liftTerminal (simplifySet a)
      DontMatch a      -> a

-- | Match a qualifier
matchPSet :: (Eq e, Eq p, Eq a, Ord p) => PredOracle p -> a -> PSet e p a -> Bool
matchPSet fp y q = case reducePredicates fp q of
   NoMatch            -> False
   MatchDiverge _     -> False
   MatchFail _        -> False
   Match s            -> y `elemSet` s
   DontMatch _        -> False

-- | Try to set the value of a PSet if it matches. Otherwise leave it untouched
trySetPSet :: (Eq e, Eq p, Eq a, Ord p) => PredOracle p -> a -> PSet e p a -> PSet e p a
trySetPSet oracle a q = fmap f q
   where
      f | matchPSet oracle a q = \_ -> Singleton a
        | otherwise            = id

-- | Try to set the value of a CSet if it matches. Otherwise leave it untouched
trySetCSet :: (Eq a) => a -> CSet a -> CSet a
trySetCSet a s
   | a `elemSet` s = Singleton a
   | otherwise     = s


-- | Make a terminal register family matching a single register
regFamFromReg :: Reg b -> TermRegFam b
regFamFromReg Reg{..} = RegFam
   { regFamBank   = Singleton registerBank
   , regFamId     = Singleton registerId
   , regFamSize   = Singleton registerSize
   , regFamOffset = Singleton registerOffset
   }

-- | Make a predicated register family matching a single register
pRegFamFromReg :: (Predicated (PredRegFam p e b)) => Reg b -> PredRegFam p e b
pRegFamFromReg = liftTerminal . regFamFromReg
