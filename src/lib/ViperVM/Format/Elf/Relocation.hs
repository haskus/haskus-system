module ViperVM.Format.Elf.Relocation
   ( RelocationEntry (..)
   , getRelocationEntry
   , putRelocationEntry
   )
where

import Data.Word
import Data.Int
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

import ViperVM.Format.Elf.PreHeader
import ViperVM.Format.Elf.Header
import ViperVM.Format.Elf.RelocationType

-- | Relocation entry
data RelocationEntry = RelocationEntry
   { relocAddress       :: Word64
   , relocType          :: RelocationType
   , relocSymbolIndex   :: Word32
   , relocAddend        :: Maybe Int64
   }
   deriving (Show)


getRelocationEntry :: PreHeader -> Header -> Bool -> Get RelocationEntry
getRelocationEntry i h withAddend = do
   let (_,_,_,gwN) = getGetters i
   
   addr <- gwN
   info <- gwN
   let
      typ = toRelocType (headerArch h) $ case preHeaderWordSize i of
         WordSize32 -> fromIntegral (info .&. 0xff)
         WordSize64 -> fromIntegral (info .&. 0xffffffff)

      sym = case preHeaderWordSize i of
         WordSize32 -> fromIntegral (info `shiftR` 8)
         WordSize64 -> fromIntegral (info `shiftR` 32)

   ad <- if withAddend
      then (Just . fromIntegral <$> gwN) 
      else return Nothing

   return $ RelocationEntry addr typ sym ad

putRelocationEntry :: PreHeader -> Bool -> RelocationEntry -> Put
putRelocationEntry i withAddend rel = do
   let 
      (_,_,_,pwN) = getPutters i
      sym = relocSymbolIndex rel
      typ = fromRelocType (relocType rel)
      info = case preHeaderWordSize i of
         WordSize32 -> (fromIntegral sym `shiftL` 8) 
                       .|. (fromIntegral typ .&. 0xff)
         WordSize64 -> (fromIntegral sym `shiftL` 32) 
                       .|. (fromIntegral typ .&. 0xffffffff)

   pwN (relocAddress rel)
   pwN info
   case (withAddend, relocAddend rel) of
      (True, Just x)   -> pwN (fromIntegral x)
      (False, Nothing) -> return ()
      _                -> error "Addend not found"

