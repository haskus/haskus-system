{-# LANGUAGE DeriveAnyClass #-}

-- | Version sections
module ViperVM.Format.Elf.Version
   ( -- * Version definition
     VersionDefinition (..)
   , VersionDefinitionVersion (..)
   , VersionDefinitionFlag (..)
   , VersionDefinitionFlags
   , VersionDefinitionAuxiliary (..)
   , VersionIndex (..)
   , getVersionDefinition
   , putVersionDefinition
   -- * Version needed
   , RawVersionNeeded (..)
   , VersionNeededVersion
   , RawVersionNeededAuxiliary (..)
   , getRawVersionNeeded
   , putRawVersionNeeded
   , getRawVersionNeededAuxiliary
   , putRawVersionNeededAuxiliary
   )
where

import Data.Word
import ViperVM.Format.Binary.Get
import ViperVM.Format.Binary.Put

import ViperVM.Format.Binary.BitSet as BitSet

import ViperVM.Format.Elf.PreHeader


data VersionDefinition = VersionDefinition
   { vdVersion         :: VersionDefinitionVersion   -- ^ Version revision
   , vdFlags           :: VersionDefinitionFlags     -- ^ Version information
   , vdIndex           :: VersionIndex               -- ^ Version index
   , vdAuxCount        :: Word16                     -- ^ Number of associated aux entries
   , vdNameHash        :: Word32                     -- ^ Version name hash value
   , vdAuxTableOffset  :: Word32                     -- ^ Offset in bytes to VersionDefinitionAuxiliary array
   , vdNext            :: Word32                     -- ^ Offset in bytes to next VersionDefinition
   }
   deriving (Show,Eq)

data VersionDefinitionVersion
   = VersionNone        -- ^ No version
   | VersionCurrent     -- ^ Current version
   | VersionGiven       -- ^ Given version number
   deriving (Show,Eq,Enum)

data VersionDefinitionFlag
   = VersionFlagBase    -- ^ Version definition of file itself
   | VersionFlagWeak    -- ^ Weak version identifier
   deriving (Show,Eq,Enum,CBitSet)

type VersionDefinitionFlags = BitSet Word16 VersionDefinitionFlag

data VersionIndex
   = VersionIndexLocal
   | VersionIndexGlobal
   | VersionIndexEliminate
   | VersionIndexCustom Word16
   deriving (Show,Eq)

fromVersionIndex :: VersionIndex -> Word16
fromVersionIndex x = case x of
   VersionIndexLocal       -> 0
   VersionIndexGlobal      -> 1
   VersionIndexEliminate   -> 0xff01
   VersionIndexCustom v    -> v

toVersionIndex :: Word16 -> VersionIndex
toVersionIndex x = case x of
   0        -> VersionIndexLocal
   1        -> VersionIndexGlobal
   0xff01   -> VersionIndexEliminate
   v        -> VersionIndexCustom v
   

getVersionDefinition :: PreHeader -> Get VersionDefinition
getVersionDefinition pre = do
   let (_,gw16,gw32,_,_) = getGetters pre

   VersionDefinition
      <$> (toEnum . fromIntegral <$> gw16)
      <*> (BitSet.fromBits <$> gw16)
      <*> (toVersionIndex <$> gw16)
      <*> gw16
      <*> gw32
      <*> gw32
      <*> gw32

putVersionDefinition :: PreHeader -> VersionDefinition -> Put
putVersionDefinition pre vd = do
   let (_,pw16,pw32,_,_) = getPutters pre

   pw16 (fromIntegral . fromEnum $ vdVersion vd)
   pw16 (BitSet.toBits $ vdFlags vd)
   pw16 (fromVersionIndex $ vdIndex vd)
   pw16 (vdAuxCount vd)
   pw32 (vdNameHash vd)
   pw32 (vdAuxTableOffset vd)
   pw32 (vdNext vd)

data VersionDefinitionAuxiliary = VersionDefinitionAuxiliary
   { vdaName   :: Word32      -- ^ Version or dependency names
   , vdaNext   :: Word32      -- ^ Offset in bytes to next VersionDefinitionAuxiliary
   }
   deriving (Show,Eq)

data RawVersionNeeded = RawVersionNeeded
   { rvnVersion  :: VersionNeededVersion  -- ^ Version of structure
   , rvnAuxCount :: Word16                -- ^ Number of associated aux entries
   , rvnFileName :: Word32                -- ^ Offset of filename for this dependency
   , rvnAuxTable :: Word32                -- ^ Offset in bytes to VersionNeededAuxiliary array
   , rvnNext     :: Word32                -- ^ Offset in bytes to next VersionNeeded 
   }
   deriving (Show,Eq)

-- Currently the current legal version values for VersionNeeded are the same as
-- for VersionDefinition
type VersionNeededVersion = VersionDefinitionVersion

getRawVersionNeeded :: PreHeader -> Get RawVersionNeeded
getRawVersionNeeded pre = do
   let (_,gw16,gw32,_,_) = getGetters pre

   RawVersionNeeded
      <$> (toEnum . fromIntegral <$> gw16)
      <*> gw16
      <*> gw32
      <*> gw32
      <*> gw32

putRawVersionNeeded :: PreHeader -> RawVersionNeeded -> Put
putRawVersionNeeded pre vn = do
   let (_,pw16,pw32,_,_) = getPutters pre

   pw16 (fromIntegral . fromEnum $ rvnVersion vn)
   pw16 (rvnAuxCount vn)
   pw32 (rvnFileName vn)
   pw32 (rvnAuxTable vn)
   pw32 (rvnNext vn)



data RawVersionNeededAuxiliary = RawVersionNeededAuxiliary
   { rvnaHash   :: Word32   -- ^ Hash value of dependency name
   , rvnaFlags  :: Word16   -- ^ Dependency specific information
   , rvnaOther  :: Word16   -- ^ Unused
   , rvnaName   :: Word32   -- ^ Dependency name string offset
   , rvnaNext   :: Word32   -- ^ Offset in bytes to next VersionNeededAuxiliary
   }
   deriving (Show,Eq)

getRawVersionNeededAuxiliary :: PreHeader -> Get RawVersionNeededAuxiliary
getRawVersionNeededAuxiliary pre = do
   let (_,gw16,gw32,_,_) = getGetters pre

   RawVersionNeededAuxiliary
      <$> gw32
      <*> gw16
      <*> gw16
      <*> gw32
      <*> gw32

putRawVersionNeededAuxiliary :: PreHeader -> RawVersionNeededAuxiliary -> Put
putRawVersionNeededAuxiliary pre aux = do
   let (_,pw16,pw32,_,_) = getPutters pre

   pw32 (rvnaHash aux)
   pw16 (rvnaFlags aux)
   pw16 (rvnaOther aux)
   pw32 (rvnaName aux)
   pw32 (rvnaNext aux)
