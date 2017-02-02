{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Aeson
import GHC.Generics
import Haskus.Utils.Variant

------------- Boilerplate --------------
class ToJSON' x where
   toJSON' :: Int -> x -> Value

instance ToJSON' (Variant '[]) where
   toJSON' = undefined

instance
   ( ToJSON' (Variant ts)
   , ToJSON t
   ) => ToJSON' (Variant (t ': ts))
   where
      toJSON' n v = case headVariant v of
         Right t -> toJSON (n,t)
         Left ts -> toJSON' (n+1) ts

instance ToJSON' (Variant ts) => ToJSON (Variant ts) where
   toJSON = toJSON' 0

------------- End boilerplate --------------


data Foo = Foo Int Int deriving (Show, Generic, FromJSON, ToJSON)
data Bar = Bar Int Int deriving (Show, Generic, FromJSON, ToJSON)
data Baz = Baz String  deriving (Show, Generic, FromJSON, ToJSON)

type T = Variant '[Foo,Bar,Baz]

main :: IO ()
main = do
   let 
      t0 = setVariant (Foo 42 43) :: T
      t1 = setVariant (Bar 42 43) :: T
      t2 = setVariant (Baz "Test") :: T
   print (encode t0)
   print (encode t1)
   print (encode t2)
