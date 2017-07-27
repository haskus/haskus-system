{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- - Immediate operand
module Haskus.Arch.Common.Immediate
   ( Imm (..)
   , ImmFam (..)
   , ImmFamT
   , ImmFamP
   , immFamToImm
   )
where

import Haskus.Format.Binary.Word
import Haskus.Arch.Common.Solver
import Haskus.Utils.Solver
import Haskus.Utils.Flow
import Haskus.Utils.List (nub)

-- | Immediate value
data Imm it s = Imm
   { immSize         :: !s          -- ^ Size of the immediate
   , immSignExtended :: !(Maybe s)  -- ^ Sign-extended to the given size
   , immValue        :: !Word64     -- ^ Value of the immediate
   , immType         :: !(Maybe it) -- ^ Type of the immediate
   }
   deriving (Show,Eq)

-- | Immediate family
data ImmFam t it s = ImmFam
   { immFamSize         :: !(Q t s)              -- ^ Size of the immediate
   , immFamSignExtended :: !(Q t (Maybe s))      -- ^ Sign-extended to the given size
   , immFamValue        :: !(Q t (Maybe Word64)) -- ^ Value of the immediate
   , immFamType         :: !(Q t (Maybe it))     -- ^ Type of the immediate
   }

-- | Predicated immediate family
type ImmFamP p e it s = ImmFam (NT p e) it s

-- | Terminal immediate family
type ImmFamT it s     = ImmFam T it s

deriving instance (Show it, Show s) => Show (ImmFam T it s)
deriving instance (Eq it, Eq s)     => Eq   (ImmFam T it s)
deriving instance (Ord it, Ord s)   => Ord  (ImmFam T it s)
deriving instance (Show p, Show e, Show it, Show s) => Show (ImmFam (NT p e) it s)
deriving instance (Eq p, Eq e, Eq it, Eq s)         => Eq   (ImmFam (NT p e) it s)
deriving instance (Ord p, Ord e, Ord it, Ord s)     => Ord  (ImmFam (NT p e) it s)

instance (Ord p, Eq e, Eq i, Eq s, Eq p) => Predicated (ImmFam (NT p e) i s) where
   type Pred     (ImmFam (NT p e) i s) = p
   type PredErr  (ImmFam (NT p e) i s) = e
   type PredTerm (ImmFam (NT p e) i s) = ImmFam T i s

   liftTerminal (ImmFam s e v t) = ImmFam (liftTerminal s)
                                          (liftTerminal e)
                                          (liftTerminal v)
                                          (liftTerminal t)

   reducePredicates oracle (ImmFam s e v t) =
      initP ImmFam ImmFam
         |> (`applyP` reducePredicates oracle s)
         |> (`applyP` reducePredicates oracle e)
         |> (`applyP` reducePredicates oracle v)
         |> (`applyP` reducePredicates oracle t)
         |> resultP

   getTerminals (ImmFam ss es vs ts) =
      [ ImmFam s e v t | s <- getTerminals ss
                       , e <- getTerminals es
                       , v <- getTerminals vs
                       , t <- getTerminals ts
      ]

   getPredicates (ImmFam s e v t) =
      nub $ concat [ getPredicates s
                   , getPredicates e
                   , getPredicates v
                   , getPredicates t
                   ]


-- | Convert an immediate family into an immediate
immFamToImm :: ImmFamT it s -> Maybe (Imm it s)
immFamToImm ImmFam{..} = case immFamValue of
   Nothing -> Nothing
   Just v  -> Just (Imm immFamSize immFamSignExtended v immFamType)
