{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haskus.System.Input
import Haskus.System.Event
import Haskus.System.Sys
import Haskus.System.Terminal
import Haskus.System.Process
import Haskus.System.Linux.Handle
import Haskus.System.Linux.FileSystem
import Haskus.Utils.Flow
import qualified Haskus.Binary.BitSet as BitSet

import System.Environment

main :: IO ()
main = runSys' <| do

   term <- defaultTerminal
   args <- liftIO getArgs
   
   case args of
      (devpath:_) -> do
         let flgs = BitSet.fromList [HandleReadWrite,HandleNonBlocking]
         hdl <- assertLogShowErrorE "Open devices" <| open Nothing devpath flgs BitSet.empty

         eventChannel  <- newEventReader hdl

         onEvent eventChannel <| \ev -> do
            let ev' = makeInputEvent ev
            case inputEventType ev' of
               InputKeyEvent action key
                  | action /= KeyRepeat -> writeStrLn term (show key ++ ": " ++ show action)
               _                        -> return ()


         threadDelaySec 20

      _ -> writeStrLn term ("Usage: sudo haskus-keys /dev/input/event0")
