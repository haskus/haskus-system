module ElfCmdLine
   ( Options(..)
   , getOptions
   )
where

import Options.Applicative

data Options = Options
   { optpath    :: String
   , optport    :: Int
   }

options :: Parser Options
options = Options
  <$> argument str (
        metavar "PATH"
     <> help "Path to the binary or the project"
     )
  <*> option auto (
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
     <> progDesc "Show information on ELF binary file"
     <> header "ELF info" )
