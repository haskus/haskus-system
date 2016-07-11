{-# LANGUAGE TemplateHaskell #-}

-- | Embed data into the executable binary
module ViperVM.Utils.Embed
   ( embedBS
   , module Data.FileEmbed
   )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH

import Data.FileEmbed

-- | Embed a bytestring into a binary
embedBS :: BS.ByteString -> Q Exp
embedBS bs =
    return $ VarE 'unsafePerformIO
      `AppE` (VarE 'unsafePackAddressLen
      `AppE` LitE (IntegerL $ fromIntegral $ B8.length bs)
      `AppE` LitE (StringPrimL $ BS.unpack bs))

