module Haskus.Apps.System.Build.CmdLine
   ( InitOptions(..)
   , BuildOptions (..)
   , TestOptions (..)
   , initOptions
   , buildOptions
   , testOptions
   )
where

import Options.Applicative
import Data.Monoid

data InitOptions = InitOptions
   { initOptTemplate :: String
   }

initOptions :: Parser InitOptions
initOptions =
   InitOptions
      <$> strOption
         (  long "template"
         <> short 't'
         <> metavar "TEMPLATE"
         <> value "default"
         <> help "Template to use"
         )

data TestOptions = TestOptions
   { testOptInit :: String
   }

testOptions :: Parser TestOptions
testOptions =
   TestOptions
      <$> strOption
         (  long "init"
         <> metavar "INIT-PROGRAM"
         <> value ""
         <> help "Init program to use"
         )

data BuildOptions = BuildOptions
   { buildOptInit :: String
   }

buildOptions :: Parser BuildOptions
buildOptions =
   BuildOptions
      <$> strOption
         (  long "init"
         <> metavar "INIT-PROGRAM"
         <> value ""
         <> help "Init program to use"
         )
