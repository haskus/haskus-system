module Haskus.Apps.System.Build.CmdLine
   ( InitOptions(..)
   , initOptions
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
