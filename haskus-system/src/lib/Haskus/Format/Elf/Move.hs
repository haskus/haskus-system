-- | Move sections
module Haskus.Format.Elf.Move
   ( MoveEntry (..)
   , getMoveEntry
   , putMoveEntry
   )
where

import Haskus.Binary.Bits
import Haskus.Number.Word
import Haskus.Binary.Get
import Haskus.Binary.Put
import Haskus.Format.Elf.PreHeader

-- | Move record
data MoveEntry = MoveEntry
   { moveValue       :: Word64      -- ^ Symbol value
   , moveSymbolIndex :: Word64      -- ^ Index
   , moveSymbolSize  :: Word8       -- ^ Size
   , moveOffset      :: Word64      -- ^ Symbol offset
   , moveRepeatCount :: Word16      -- ^ Repeat count
   , moveStride      :: Word16      -- ^ Stride info
   }
   deriving (Show,Eq)

-- | Getter for a move entry
getMoveEntry :: PreHeader -> Get MoveEntry
getMoveEntry pre = do
   let (_,gw16,_,gw64,gwN) = getGetters pre

   value <- gw64
   info  <- gwN

   MoveEntry
      value
      (info `shiftR` 8)
      (fromIntegral $ info .&. 0xff)
      <$> gwN
      <*> gw16
      <*> gw16

-- | Putter for a move entry
putMoveEntry :: PreHeader -> MoveEntry -> Put
putMoveEntry pre e = do
   let 
      (_,pw16,_,pw64,pwN) = getPutters pre
      info = (moveSymbolIndex e `shiftL` 8)
             .|. fromIntegral (moveSymbolSize e)

   pw64 (moveValue e)
   pwN  info
   pwN  (moveOffset e)
   pw16 (moveRepeatCount e)
   pw16 (moveStride e)
