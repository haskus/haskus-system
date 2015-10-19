module CmdLine
   ( Options(..)
   , getOptions
   )
where

import Options.Applicative

data Options = Options
   { optport    :: Int
   }

options :: Parser Options
options = Options
  <$> option auto (
        long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8000
     <> help "Use port PORT for the HTTP server"
     )
  

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Show platform information"
     <> header "Platform info" )
