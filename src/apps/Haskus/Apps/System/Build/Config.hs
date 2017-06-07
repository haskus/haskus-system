{-# LANGUAGE OverloadedStrings #-}

module Haskus.Apps.System.Build.Config
   ( readSystemConfig
   , SystemConfig (..)
   , LinuxConfig (..)
   , LinuxSource (..)
   , LinuxOptions (..)
   , SyslinuxConfig (..)
   , RamdiskConfig (..)
   )
where

import Data.Yaml as Yaml
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative

-- | System configuration
data SystemConfig = SystemConfig
   { linuxConfig    :: LinuxConfig     -- ^ Linux configuration
   , syslinuxConfig :: SyslinuxConfig  -- ^ Syslinux configuration
   , ramdiskConfig  :: RamdiskConfig   -- ^ Ramdisk configuration
   }
   deriving (Show)

instance FromJSON SystemConfig where
   parseJSON (Yaml.Object v) =
      SystemConfig
         <$> (v .: "linux")
         <*> (v .:? "syslinux" .!= defaultSyslinuxConfig)
         <*> (v .: "ramdisk")

   parseJSON _ = fail "Invalid config file"

readSystemConfig :: FilePath -> IO (Maybe SystemConfig)
readSystemConfig = Yaml.decodeFile

-------------------------------------------------------------
-- Linux
-------------------------------------------------------------


-- | Linux source
data LinuxSource
   = LinuxTarball Text  -- ^ Linux x.y.z from kernel.org tarballs
   | LinuxGit Text Text -- ^ repository/commit hash
   deriving (Show)

data LinuxOptions = LinuxOptions
   { enableOptions  :: [Text]
   , disableOptions :: [Text]
   , moduleOptions  :: [Text]
   }
   deriving (Show)

-- | Linux configuration
data LinuxConfig = LinuxConfig
   { linuxSource   :: LinuxSource  -- ^ How to retrieve Linux
   , linuxOptions  :: LinuxOptions -- ^ Configuration options
   , linuxMakeArgs :: Text         -- ^ Make arguments
   }
   deriving (Show)

instance FromJSON LinuxConfig where
   parseJSON (Yaml.Object v) = do
      src     <- parseSource <$> (v .:? "source" .!= "tarball")
      options <- parseOptions <$> (v .:? "options")
      LinuxConfig
         <$> src
         <*> options
         <*> (v .:? "make-args" .!= "-j8")
      where
         parseSource :: Text -> Parser LinuxSource
         parseSource s = case s of
            "tarball" -> LinuxTarball <$> v .: "version"
            "git"     -> LinuxGit     <$> v .: "repository" <*> v .: "commit"
            r         -> fail $ "Invalid Linux source: " ++ show r

         parseOptions :: Maybe Yaml.Value -> Parser LinuxOptions
         parseOptions s = case s of
            Nothing                -> pure (LinuxOptions [] [] [])
            Just (Yaml.Object opt) ->
               LinuxOptions
                  <$> (opt .:? "enable"  .!= [])
                  <*> (opt .:? "disable" .!= [])
                  <*> (opt .:? "module"  .!= [])
            Just _ -> fail "Invalid Linux options"

            
   parseJSON _ = fail "Invalid Linux configuration"

-------------------------------------------------------------
-- Syslinux
-------------------------------------------------------------

-- | Syslinux configuration
data SyslinuxConfig = SyslinuxConfig
   { syslinuxVersion  :: Text     -- ^ Syslinux version
   }
   deriving (Show)

-- | Default Syslinux configuration
defaultSyslinuxConfig :: SyslinuxConfig
defaultSyslinuxConfig = SyslinuxConfig "6.03"

instance FromJSON SyslinuxConfig where
   parseJSON (Yaml.Object v) =
      SyslinuxConfig
         <$> (v .:? "version" .!= "6.03")

   parseJSON _ = fail "Invalid Syslinux configuration"


-------------------------------------------------------------
-- Ramdisk
-------------------------------------------------------------

-- | Ramdisk configuration
data RamdiskConfig = RamdiskConfig
   { ramdiskFileName :: Text   -- ^ Name of the ramdisk file
   , ramdiskInit     :: Text   -- ^ Init program
   }
   deriving (Show)

instance FromJSON RamdiskConfig where
   parseJSON (Yaml.Object v) = do
      let
         rdinit = v .: "init"
         rdname = (v .: "name")
                     <|> (Text.append ".img" <$> rdinit)

      RamdiskConfig
         <$> rdname
         <*> rdinit

   parseJSON _ = fail "Invalid Ramdisk configuration"

