{-# LANGUAGE TypeFamilies #-}

module Haskus.Arch.X86_64.ISA.Put
  ( Put (..)
  )
where

import Haskus.Arch.X86_64.ISA.Output

import Data.Word
import Data.Int

class Put a where
  type PutResult a
  put :: Output m => a -> m (PutResult a)

instance Put Word8  where
  type PutResult Word8 = ()
  put = putW8

instance Put Word16 where
  type PutResult Word16 = ()
  put = putW16

instance Put Word32 where
  type PutResult Word32 = ()
  put = putW32

instance Put Word64 where
  type PutResult Word64 = ()
  put = putW64

instance Put Int8   where
  type PutResult Int8 = ()
  put = putI8

instance Put Int16  where
  type PutResult Int16 = ()
  put = putI16

instance Put Int32  where
  type PutResult Int32 = ()
  put = putI32

instance Put Int64  where
  type PutResult Int64 = ()
  put = putI64


