import Haskus.System

import Haskus.Arch.Linux.FileSystem

main :: IO ()
main = runSys' <| do
   syncAll    -- flush the files
   restart    -- restart the computer
