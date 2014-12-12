module ViperVM.Utils.Hash
   ( hashString
   )
where

import Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Builder
import Data.Serialize (encode)
import Data.Digest.Pure.MD5 (md5)

-- | Hash a String (MD5)
-- TODO: use Text instead of String
hashString :: String -> String
hashString = LBS.unpack . toLazyByteString . byteStringHex . encode . md5 . LBS.pack
