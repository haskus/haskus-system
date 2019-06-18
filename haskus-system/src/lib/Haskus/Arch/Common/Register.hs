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
   -- * Register family
   , RegFam (..)
   , RegFamP
   , RegFamT
   , regMatchFamily
   , regFixupPredFamily
   , regFixupPredFamilyMaybe
   , regFamToReg
   , regFamToReg'
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

import Haskus.Arch.Common.Solver
import Haskus.Utils.Solver
import Haskus.Utils.Flow
import Haskus.Utils.Maybe
import Haskus.Utils.List (nub)

-- | Register
data Reg banks rt = Reg
   { registerBank   :: banks                -- ^ Register bank
   , registerId     :: {-# UNPACK #-} !Word -- ^ Register ID
   , registerSize   :: {-# UNPACK #-} !Word -- ^ Register size in bits
   , registerOffset :: {-# UNPACK #-} !Word -- ^ Register offset in bits (alias register)
   , registerType   :: !rt                  -- ^ Register type
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

-- | Register family
data RegFam t banks rt = RegFam
   { regFamBank   :: !(Q t (CSet banks)) -- ^ Register bank
   , regFamId     :: !(Q t (CSet Word))  -- ^ Register ID
   , regFamSize   :: !(Q t (CSet Word))  -- ^ Register size in bits
   , regFamOffset :: !(Q t (CSet Word))  -- ^ Register offset in bits
   , regFamType   :: !(Q t rt)           -- ^ Register type
   }

-- | Predicated register family
type RegFamP p e b rt = RegFam (NT p e) b rt

-- | Terminal register family
type RegFamT b rt     = RegFam T b rt

deriving instance (Show b, Show rt) => Show (RegFam T b rt)
deriving instance (Eq b, Eq rt)     => Eq   (RegFam T b rt)
deriving instance (Ord b, Ord rt)   => Ord  (RegFam T b rt)
deriving instance (Show p, Show e, Show b, Show rt) => Show (RegFam (NT p e) b rt)
deriving instance (Eq p, Eq e, Eq b, Eq rt)         => Eq   (RegFam (NT p e) b rt)
deriving instance (Ord p, Ord e, Ord b, Ord rt)     => Ord  (RegFam (NT p e) b rt)

instance (Ord p, Eq e, Eq b, Eq p, Eq rt) => Predicated (RegFam (NT p e) b rt) where
   type Pred     (RegFam (NT p e) b rt) = p
   type PredErr  (RegFam (NT p e) b rt) = e
   type PredTerm (RegFam (NT p e) b rt) = RegFam T b rt

   liftTerminal (RegFam b i s o t) = RegFam (liftTerminal b)
                                            (liftTerminal i)
                                            (liftTerminal s)
                                            (liftTerminal o)
                                            (liftTerminal t)

   reducePredicates oracle (RegFam b i s o t) =
      initP RegFam RegFam
         |> (`applyP` reducePredicates oracle b)
         |> (`applyP` reducePredicates oracle i)
         |> (`applyP` reducePredicates oracle s)
         |> (`applyP` reducePredicates oracle o)
         |> (`applyP` reducePredicates oracle t)
         |> resultP

   simplifyPredicates oracle (RegFam b i s o t) =
      RegFam (simplifyPredicates oracle b)
             (simplifyPredicates oracle i)
             (simplifyPredicates oracle s)
             (simplifyPredicates oracle o)
             (simplifyPredicates oracle t)

   getTerminals (RegFam bs is ss os ts) =
      [ RegFam b i s o t | b <- getTerminals bs
                         , i <- getTerminals is
                         , s <- getTerminals ss
                         , o <- getTerminals os
                         , t <- getTerminals ts
      ]
   getPredicates (RegFam b i s o t) =
      nub $ concat [ getPredicates b
                   , getPredicates i
                   , getPredicates s
                   , getPredicates o
                   , getPredicates t
                   ]


-- | Test if a register match a family
regMatchFamily ::
   ( Ord p
   , Eq b
   , Eq rt
   , Predicated (RegFamP p e b rt)
   ) => PredOracle p -> RegFamP p e b rt -> Reg b rt -> Bool
regMatchFamily oracle rf Reg{..} =
   case reducePredicates oracle rf of
      Match (RegFam {..}) -> registerBank `elemSet` regFamBank
                              && registerId `elemSet` regFamId
                              && registerSize `elemSet` regFamSize
                              && registerOffset `elemSet` regFamOffset
      _ -> False

-- | Fixup a family (all fields must reduce to Singleton set)
regFixupPredFamily ::
   ( Predicated (RegFamP p e b rt)
   , Show (RegFamP p e b rt)
   , Eq b
   , Eq rt
   , Ord p
   ) => PredOracle p -> RegFamP p e b rt -> Reg b rt
regFixupPredFamily oracle fam =
   case regFixupPredFamilyMaybe oracle fam of
      Nothing -> error ("Cannot fixup family: " ++ show fam)
      Just c  -> c

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFixupPredFamilyMaybe ::
   ( Predicated (RegFamP p e b rt)
   , Eq b
   , Eq rt
   , Ord p
   ) => PredOracle p -> RegFamP p e b rt -> Maybe (Reg b rt)
regFixupPredFamilyMaybe oracle fam =
   case reducePredicates oracle fam of
      Match r -> regFamToReg r
      _       -> Nothing

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFamToReg :: (Eq b, Eq rt) => RegFamT b rt -> Maybe (Reg b rt)
regFamToReg (RegFam a b c d e) =
   case (simplifySet a, simplifySet b, simplifySet c, simplifySet d) of
      (Singleton u, Singleton v, Singleton w, Singleton x)
         | w == 0    -> Nothing -- we use a size of zero to encode pairs of registers where a register may not be present (e.g. AX, DX:AX, EDX:EAX)
         | otherwise -> Just (Reg u v w x e)
      _  -> Nothing

-- | Try to fixup a family (all fields must reduce to Set qualifier)
regFamToReg' :: (Show (RegFamT b rt), Eq b, Eq rt) => RegFamT b rt -> Reg b rt
regFamToReg' fam = fromMaybe err (regFamToReg fam)
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
regFamFromReg :: Reg b rt -> RegFamT b rt
regFamFromReg Reg{..} = RegFam
   { regFamBank   = Singleton registerBank
   , regFamId     = Singleton registerId
   , regFamSize   = Singleton registerSize
   , regFamOffset = Singleton registerOffset
   , regFamType   = registerType
   }

-- | Make a predicated register family matching a single register
pRegFamFromReg :: (Predicated (RegFamP p e b rt)) => Reg b rt -> RegFamP p e b rt
pRegFamFromReg = liftTerminal . regFamFromReg
