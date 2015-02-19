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

main :: IO ()
main = do

   putStrLn "Booting HaskOS"

   let ioctl = drmIoctl sysIoctl

   -------------------------------------
   -- Try to display something on screen
   
   -- Open device
   sysOpen "/dev/dri/card0" [OpenReadWrite,CloseOnExec] [] >>= \case
      Left err -> putStrLn $ "Cannot find video card:" ++ show err
      Right fd -> getCard ioctl fd >>= \case
         Left err -> putStrLn $ "Invalid card: " ++ show err
         Right card -> do
            conns <- cardConnectors ioctl card

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
            -- create a dumb buffer
            db <- createDumbBuffer ioctl fd width height bpp dbFlags
            return ()

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
