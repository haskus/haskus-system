{-# LANGUAGE LambdaCase #-}
import ViperVM.Arch.X86_64.Linux.Power
import ViperVM.Arch.X86_64.Linux.FileSystem
import ViperVM.Arch.Linux.ErrorCode

import ViperVM.Arch.Linux.Graphics
import ViperVM.Arch.Linux.Graphics.Card
import ViperVM.Arch.Linux.Graphics.DumbBuffer
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Connector

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do

   putStrLn "Booting HaskOS"

   let 
      ioctl = drmIoctl sysIoctl
      try str a = EitherT (a >>= \case
         Left err -> return (Left (str,err))
         Right v  -> return (Right v))

   -------------------------------------
   -- Try to display something on screen
   
   ret <- runEitherT $ do
      fd    <- try "Open graphic card descriptor" $
                  sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] []
      card  <- try "Get card information from descriptor" $
                  getCard ioctl fd
      cap   <- try "Get DumbBuffer capability" $
                  cardCapability ioctl card CapDumbBuffer
      hoistEither $ if cap == 0
         then Left ("Test DumbBuffer capability", ENOENT) 
         else Right ()

      conns <- liftIO $ cardConnectors ioctl card
      hoistEither $ if null conns 
         then Left ("Get connectors", ENOENT) 
         else Right ()

      let
         isValid x  = connConnection x == Connected
                      && not (null $ connModes x)
         validConns = filter isValid conns

         -- select first connector
         conn = head validConns

         -- select highest mode
         mode = head (connModes conn)
         width  = fromIntegral $ modeHorizontalDisplay mode
         height = fromIntegral $ modeVerticalDisplay mode
         bpp    = 32
         dbFlags = 0

      dumb <- try "Create a dumb buffer" $
                  createDumbBuffer ioctl fd width height bpp dbFlags
      return (Right dumb)

   case ret of
      Left (str,err) -> putStrLn $ "Error while trying to " ++ str ++ " (" ++ show err ++ ")"
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
