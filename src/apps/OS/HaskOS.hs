{-# LANGUAGE LambdaCase #-}

import qualified ViperVM.Format.Binary.BitSet as BitSet

import ViperVM.Arch.Linux.Power
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.Error
import ViperVM.System.System

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Capability
import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Connector

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do

   putStrLn "Booting HaskOS"

   let 
      ioctl = drmIoctl sysIoctl

   runSys' $ sysLogSequence "Init graphics" $ do
      sys   <- systemInit "/system"

      fd    <- sysCallAssert "Open graphic card descriptor" $
                  sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] BitSet.empty
      card  <- sysCallAssert "Get card information from descriptor" $
                  getCard ioctl fd
      cap   <- sysCallAssert "Get GenericBuffer capability" $
                  cardCapability card CapGenericBuffer
      sysAssert "GenericBuffer capability supported" (cap /= 0)

      conns <- liftIO $ cardConnectors card
      sysAssert "Connectors available" (not (null conns))

      let
         isValid x  = connectorState x == Connected
                      && not (null $ connectorModes x)
         validConns = filter isValid conns

         -- select first connector
         conn = head validConns

         -- select highest mode
         mode = head (connectorModes conn)
         width  = fromIntegral $ modeHorizontalDisplay mode
         height = fromIntegral $ modeVerticalDisplay mode
         bpp    = 32
         dbFlags = 0

      dumb <- sysCallAssert "Create a dumb buffer" $
                  cardCreateGenericBuffer card width height bpp dbFlags

      liftIO $ putStrLn "Screen initialized"
      return dumb

   putStrLn "Press a key to continue"
   waitKey

   putStrLn "And now, shutting down"
   -- sysCallAssert "Synchronize files" $ sysSync
   -- sysCallAssert "Power off" $ sysPower PowerOff


waitKey :: IO ()
waitKey = getChar >> return ()
