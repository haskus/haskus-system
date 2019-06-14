{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Register
module Haskus.Arch.Common.Memory
   ( Mem (..)
   , MemFam (..)
   , MemFamP
   , MemFamT
   , memFamToMem
   )
where

import Haskus.Arch.Common.Solver
import Haskus.Utils.Solver
import Haskus.Utils.Flow
import Haskus.Utils.List (nub)

-- | Data in memory 
data Mem addr mtype = Mem
   { memAddr :: addr                 -- ^ Memory address
   , memType :: mtype                -- ^ Memory type
   , memSize :: {-# UNPACK #-} !Word -- ^ Memory size in bits
   }
   deriving (Show,Eq,Ord)


-- | Memory family
--
-- All the fields are predicated.
-- The memory address may be set or not (it is set in implicit operands for
-- instance). The size may be set or not (it can't be set for instructions
-- depending on runtime values, e.g., XSAVE state).
data MemFam t addr mtype = MemFam
   { memFamAddr :: !(Q t (Maybe addr))  -- ^ Memory address
   , memFamType :: !(Q t mtype)         -- ^ Memory type
   , memFamSize :: !(Q t (Maybe Word))  -- ^ Memory size in bits
   }

-- | Convert a memory family to a memory
memFamToMem :: MemFamT a m -> Maybe (Mem a m)
memFamToMem MemFam{..} =
   Mem <$> memFamAddr
       <*> Just memFamType
       <*> memFamSize

-- | Predicated memory family
type MemFamP p e a m = MemFam (NT p e) a m

-- | Terminal memory family
type MemFamT a m     = MemFam T a m

deriving instance (Show a, Show m) => Show (MemFam T a m)
deriving instance (Eq a, Eq m)     => Eq   (MemFam T a m)
deriving instance (Ord a, Ord m)   => Ord  (MemFam T a m)
deriving instance (Show p, Show e, Show a, Show m) => Show (MemFam (NT p e) a m)
deriving instance (Eq p, Eq e, Eq a, Eq m)         => Eq   (MemFam (NT p e) a m)
deriving instance (Ord p, Ord e, Ord a, Ord m)     => Ord  (MemFam (NT p e) a m)

instance (Ord p, Eq e, Eq a, Eq m, Eq p) => Predicated (MemFam (NT p e) a m) where
   type Pred     (MemFam (NT p e) a m) = p
   type PredErr  (MemFam (NT p e) a m) = e
   type PredTerm (MemFam (NT p e) a m) = MemFam T a m

   liftTerminal (MemFam a t s) = MemFam (liftTerminal a)
                                        (liftTerminal t)
                                        (liftTerminal s)

   reducePredicates oracle (MemFam a t s) =
      initP MemFam MemFam
         |> (`applyP` reducePredicates oracle a)
         |> (`applyP` reducePredicates oracle t)
         |> (`applyP` reducePredicates oracle s)
         |> resultP

   simplifyPredicates oracle (MemFam a t s) =
      MemFam (simplifyPredicates oracle a)
             (simplifyPredicates oracle t)
             (simplifyPredicates oracle s)

   getTerminals (MemFam as ts ss) =
      [ MemFam a t s| a <- getTerminals as
                    , t <- getTerminals ts
                    , s <- getTerminals ss
      ]

   getPredicates (MemFam a t s) =
      nub $ concat [ getPredicates a
                   , getPredicates t
                   , getPredicates s
                   ]

