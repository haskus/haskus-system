{-# LANGUAGE DataKinds #-}

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
module Haskus.System.Linux.Devices
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

import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Handle
import Haskus.System.Linux.FileSystem
import Haskus.System.Linux.FileSystem.ReadWrite
import Haskus.System.Linux.FileSystem.SymLink

import qualified Haskus.Format.Binary.BitSet as BitSet
import qualified Haskus.Format.Text as Text
import Haskus.Format.Text (Text)
import Haskus.Format.Binary.Word
import Haskus.Utils.Flow
import Haskus.System.FileSystem

import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Void

type Parser = Parsec Void Text

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
createDeviceFile :: MonadInIO m => Maybe Handle -> FilePath -> Device -> FilePermissions -> Excepts '[ErrorCode] m ()
createDeviceFile hdl path dev perm = sysCreateSpecialFile hdl path typ perm (Just devid)
   where
      devid = deviceID dev
      typ   = case deviceType dev of
                  CharDevice  -> FileTypeCharDevice
                  BlockDevice -> FileTypeBlockDevice

-- parser for dev files
-- content format is: MMM:mmm\n (where M is major and m is minor)
parseDevFile :: Parser DeviceID
parseDevFile = do
   major <- decimal
   void (char ':')
   minor <- decimal
   void eol
   return (DeviceID major minor)

-- | Read device major and minor in "dev" file
sysfsReadDevFile' :: MonadIO m => Handle -> Excepts ReadErrors' m DeviceID
sysfsReadDevFile' devfd = do
   -- 16 bytes should be enough
   content <- handleReadBuffer devfd Nothing 16
   case parse parseDevFile "" (Text.bufferDecodeUtf8 content) of
      Right x -> return x
      --FIXME: return a ParseError instead
      Left _  -> error "Invalid dev file format"

-- | Read device major and minor from device path
sysfsReadDevFile :: MonadInIO m => Handle -> FilePath -> m (Maybe DeviceID)
sysfsReadDevFile hdl path = do
   withOpenAt hdl (path </> "dev") BitSet.empty BitSet.empty sysfsReadDevFile'
      ||> Just
      |> catchEvalE (const (return Nothing))

-- | Read subsystem link
sysfsReadSubsystem :: MonadInIO m => Handle -> FilePath -> m (Maybe Text)
sysfsReadSubsystem hdl path = do
   readSymbolicLink (Just hdl) (path </> "subsystem")
      -- on success, only keep the basename as it is the subsystem name
      ||> Just . Text.pack . takeBaseName
      -- otherwise
      |> catchEvalE (const (return Nothing))

-- | Make a Device from a subsystem and a DeviceID
sysfsMakeDev :: Text -> DeviceID -> Device
sysfsMakeDev subsystem devid = case Text.unpack subsystem of
   "block" -> Device BlockDevice devid
   _       -> Device CharDevice  devid

-- | Read device and subsystem
sysfsReadDev :: MonadInIO m => Handle -> FilePath -> m (Maybe Text, Maybe Device)
sysfsReadDev hdl path = do
   subsystem <- sysfsReadSubsystem hdl path
   case subsystem of
      Nothing -> return (Nothing,Nothing)
      Just s  -> do
         devid <- sysfsReadDevFile hdl path
         return (Just s, sysfsMakeDev s <$> devid)
