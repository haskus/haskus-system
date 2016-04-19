-- | Note sections
module ViperVM.Format.Elf.Note
   ( RawNote (..)
   , getRawNote
   , putRawNote
   )
where


import Data.Word
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put

import ViperVM.Format.Elf.PreHeader

-- | Note
data RawNote = RawNote
   { rawnoteNameLength        :: Word32
   , rawnoteDescriptorSize    :: Word32
   , rawnoteType              :: Word32
   }
   deriving (Show,Eq)

-- | Getter for a note
getRawNote :: PreHeader -> Get RawNote
getRawNote pre = do
   let (_,_,gw32,_,_) = getGetters pre

   RawNote
      <$> gw32
      <*> gw32
      <*> gw32

-- | Putter for a note
putRawNote :: PreHeader -> RawNote -> Put
putRawNote pre note = do
   let (_,_,pw32,_,_) = getPutters pre

   pw32 (rawnoteNameLength note)
   pw32 (rawnoteDescriptorSize note)
   pw32 (rawnoteType note)
