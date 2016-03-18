{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Variant type
module ViperVM.Utils.Variant
   ( Variant
   , getVariant
   , setVariant
   )
where

import GHC.TypeLits
import Unsafe.Coerce
import Data.Proxy

-- | A variant contains a value whose type is at the given position in the type
-- list
data Variant (l :: [*]) = forall a . Variant Int a


type family TypeAt (n :: Nat) l where
   TypeAt 0 (x ': xs) = x
   TypeAt n (x ': xs) = TypeAt (n-1) xs

-- | Get the value if it has the indexed type
getVariant :: forall (n :: Nat) l . 
   (KnownNat n)
   => Proxy n -> Variant l -> Maybe (TypeAt n l)
getVariant _ (Variant t a) =
   if t /= fromIntegral (natVal (Proxy :: Proxy n))
      then Nothing
      else Just (unsafeCoerce a) -- we know it is the effective type


-- | Set the value with the given indexed type
setVariant :: forall (n :: Nat) l .
   (KnownNat n)
   => Proxy n -> TypeAt n l -> Variant l
setVariant _ = Variant (fromIntegral (natVal (Proxy :: Proxy n)))
