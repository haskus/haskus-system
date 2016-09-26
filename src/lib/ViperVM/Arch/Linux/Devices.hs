{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Linux device handling
--
-- Devices in the kernel are identified with two numbers (major and minor) and
-- their type (character or block).
--
-- For each device, there is a 1-1 correspondance with some paths in sysfs's
-- /devices directory:
--
--    * type/major/minor -> sysfs path
--       Look at target of symbolic link /dev/{block,char}/MAJOR:MINOR
--
--    * sysfs path -> type/major/minor
--       In the sysfs's device directory (/devices/**):
--          * type: if basename of "subsystem" link is "block" then block else
--          character
--          * major/minor: read contents of "dev" file
--
module ViperVM.Arch.Linux.Devices
   ( Device(..)
   , showDevice
   , makeDevice
   , DeviceType(..)
   , DeviceID(..)
   , createDeviceFile
   , sysfsReadDevFile
   , sysfsReadDev
   , sysfsMakeDev
   , sysfsReadSubsystem
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Handle
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.FileSystem.SymLink

import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Format.Text as Text
import ViperVM.Format.Binary.Word
import ViperVM.Utils.Flow
import ViperVM.System.Sys
import ViperVM.System.FileSystem

import System.FilePath
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)


-- | Device
data Device = Device
   { deviceType :: !DeviceType               -- ^ Device type
   , deviceID   :: {-# UNPACK #-} !DeviceID  -- ^ Device major and minor
   }
   deriving (Show,Eq,Ord)

-- | Create a device identigier
makeDevice :: DeviceType -> Word32 -> Word32 -> Device
makeDevice typ major minor = Device typ (DeviceID major minor)

-- | Show a device as a path in sysfs /dev
showDevice :: Device -> String
showDevice (Device typ (DeviceID ma mi)) =
   "/dev/" ++ typ' ++ "/" ++ show ma ++ ":" ++ show mi
   where
      typ' = case typ of
               CharDevice  -> "char"
               BlockDevice -> "block"

-- | Device type
data DeviceType
   = CharDevice   -- ^ Character device
   | BlockDevice  -- ^ Block device
   deriving (Show,Eq,Ord)

-- | Create a device special file
createDeviceFile :: Maybe Handle -> FilePath -> Device -> FilePermissions -> SysRet ()
createDeviceFile hdl path dev perm = sysCreateSpecialFile hdl path typ perm (Just devid)
   where
      devid = deviceID dev
      typ   = case deviceType dev of
                  CharDevice  -> FileTypeCharDevice
                  BlockDevice -> FileTypeBlockDevice


-- | Read device major and minor in "dev" file
sysfsReadDevFile' :: Handle -> SysV '[DeviceID,ErrorCode]
sysfsReadDevFile' devfd = do
   let
      -- parser for dev files
      -- content format is: MMM:mmm\n (where M is major and m is minor)
      parseDevFile :: Parsec Text DeviceID
      parseDevFile = do
         major <- fromIntegral <$> decimal
         void (char ':')
         minor <- fromIntegral <$> decimal
         void eol
         return (DeviceID major minor)

   -- 16 bytes should be enough
   sysIO (handleReadBuffer devfd Nothing 16)
      >.-.> (\content -> case parseMaybe parseDevFile (bufferDecodeUtf8 content) of
         Nothing -> error "Invalid dev file format"
         Just x  -> x)

-- | Read device major and minor from device path
sysfsReadDevFile :: Handle -> FilePath -> Sys (Maybe DeviceID)
sysfsReadDevFile hdl path = do
   withOpenAt hdl (path </> "dev") BitSet.empty BitSet.empty sysfsReadDevFile'
      >.-.> Just
      >..-.> const Nothing
      |> flowRes

-- | Read subsystem link
sysfsReadSubsystem :: Handle -> FilePath -> Sys (Maybe Text)
sysfsReadSubsystem hdl path = do
   readSymbolicLink (Just hdl) (path </> "subsystem")
      -- on success, only keep the basename as it is the subsystem name
      >.-.> Just . Text.pack . takeBaseName
      -- otherwise
      >..-.> const Nothing
      |> flowRes

-- | Make a Device from a subsystem and a DeviceID
sysfsMakeDev :: Text -> DeviceID -> Device
sysfsMakeDev subsystem devid = case Text.unpack subsystem of
   "block" -> Device BlockDevice devid
   _       -> Device CharDevice  devid

-- | Read device and subsystem
sysfsReadDev :: Handle -> FilePath -> Sys (Maybe Text, Maybe Device)
sysfsReadDev hdl path = do
   subsystem <- sysfsReadSubsystem hdl path
   case subsystem of
      Nothing -> return (Nothing,Nothing)
      Just s  -> do
         devid <- sysfsReadDevFile hdl path
         return (Just s, sysfsMakeDev s <$> devid)
