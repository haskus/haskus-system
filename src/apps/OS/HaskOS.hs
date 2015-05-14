{-# LANGUAGE LambdaCase #-}

import qualified ViperVM.Utils.BitSet as BitSet

import ViperVM.Arch.Linux.Power
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.System.System

import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.Capability
import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Connector

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do

   putStrLn "Booting HaskOS"

   let 
      ioctl = drmIoctl sysIoctl

   ret <- runCatch $ do
      sys   <- sysTry "Initialize System" $ systemInit "/system"

      fd    <- sysTry "Open graphic card descriptor" $
                  sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] BitSet.empty
      card  <- sysTry "Get card information from descriptor" $
                  getCard ioctl fd
      cap   <- sysTry "Get GenericBuffer capability" $
                  cardCapability card CapGenericBuffer
      hoistEither $ if cap == 0
         then Left ("Test GenericBuffer capability", ENOENT) 
         else Right ()

      conns <- liftIO $ cardConnectors card
      hoistEither $ if null conns 
         then Left ("Get connectors", ENOENT) 
         else Right ()

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

      dumb <- sysTry "Create a dumb buffer" $
                  cardCreateGenericBuffer card width height bpp dbFlags
      return (Right dumb)

   case ret of
      Left _  -> return ()
      Right _ -> putStrLn "Screen initialized"

   putStrLn "Press a key to continue"
   waitKey

   putStrLn "And now, shutting down"
   check $ sysSync
   check $ sysPower PowerOff


waitKey :: IO ()
waitKey = getChar >> return ()

check :: SysRet a -> IO a
check f = do
   r <- f
   case r of
      Left err -> do
         error (show err)
      Right v -> return v
