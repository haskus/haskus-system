{-# LANGUAGE TemplateHaskell #-}

-- | Embed data into the executable binary
module ViperVM.Utils.Embed
   ( embedBS
   , embedBytes
   , embedBuffer
   , module Data.FileEmbed
   )
where

import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH
import Data.FileEmbed

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Buffer

-- | Embed a [Word8] into a binary, return a ByteString
embedBS :: [Word8] -> Q Exp
embedBS bs =
   return $ VarE 'unsafePerformIO
     `AppE` (VarE 'unsafePackAddressLen
     `AppE` LitE (IntegerL $ fromIntegral $ length bs)
     `AppE` LitE (StringPrimL bs))

-- | Embed a [Word8] into a binary, return a Buffer
embedBuffer :: [Word8] -> Q Exp
embedBuffer bs = do
   bs' <- embedBS bs
   return $ VarE 'bufferPackByteString
      `AppE` bs'

-- | Embed bytes in a C array, return an Addr#
embedBytes :: [Word8] -> Q Exp
embedBytes bs = return $ LitE (StringPrimL bs)
