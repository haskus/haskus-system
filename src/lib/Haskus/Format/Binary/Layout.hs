{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Memory layout
--
-- Describe a memory region
module Haskus.Format.Binary.Layout
   ( LayoutPathType
   , LayoutPathOffset
   , LayoutRoot
   , LayoutPath (..)
   , LayoutIndex (..)
   , LayoutSymbol (..)
   , layoutIndex
   , layoutSymbol
   , (:->)
   , (:#>)
   )
where

import Haskus.Utils.Types
import Haskus.Utils.Types.List

-- | Path in a layout
data LayoutPath (path :: [*])   = LayoutPath

-- | Index in a layout path
data LayoutIndex (n :: Nat)     = LayoutIndex

-- | Symbol in a layout path
data LayoutSymbol (s :: Symbol) = LayoutSymbol

-- | Index in the layout path
layoutIndex :: forall n. LayoutPath '[LayoutIndex n]
layoutIndex = LayoutPath

-- | Symbol in the layout path
layoutSymbol :: forall s. LayoutPath '[LayoutSymbol s]
layoutSymbol = LayoutPath


-- | Type obtained when following path p
type family LayoutPathType l p :: *
type instance LayoutPathType l (LayoutPath '[])  = l

-- | Offset obtained when following path p
type family LayoutPathOffset l p :: Nat
type instance LayoutPathOffset e (LayoutPath '[])  = 0

type LayoutRoot = LayoutPath '[]

type family (:->) p (s :: Symbol) where
   (:->) (LayoutPath xs) s = LayoutPath (Snoc xs (LayoutSymbol s))

type family (:#>) p (n :: Nat) where
   (:#>) (LayoutPath xs) n = LayoutPath (Snoc xs (LayoutIndex n))
