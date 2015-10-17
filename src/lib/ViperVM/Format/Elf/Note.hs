-- | Note sections
module ViperVM.Format.Elf.Note
   ( RawNote (..)
   , getRawNote
   , putRawNote
   )
where


import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import ViperVM.Format.Elf.PreHeader

data RawNote = RawNote
   { rawnoteNameLength        :: Word32
   , rawnoteDescriptorSize    :: Word32
   , rawnoteType              :: Word32
   }
   deriving (Show,Eq)

getRawNote :: PreHeader -> Get RawNote
getRawNote pre = do
   let (_,gw32,_,_) = getGetters pre

   RawNote
      <$> gw32
      <*> gw32
      <*> gw32

putRawNote :: PreHeader -> RawNote -> Put
putRawNote pre note = do
   let (_,pw32,_,_) = getPutters pre

   pw32 (rawnoteNameLength note)
   pw32 (rawnoteDescriptorSize note)
   pw32 (rawnoteType note)

