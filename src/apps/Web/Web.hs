{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import ViperVM.Platform.Host as V
import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Loading
import ViperVM.Platform.Config
import ViperVM.Platform.Memory
import ViperVM.Platform.Memory.Buffer
import ViperVM.Platform.Topology as V
import ViperVM.Arch.OpenCL.All as CL
import qualified ViperVM.STM.TSet as TSet

import Paths_ViperVM
import Data.Version

import Control.Concurrent.STM
import qualified Data.Set as Set

import Control.Applicative ((<$>))
import Control.Monad (msum, forM_, guard)
import Control.Monad.Trans.Class (lift)
import System.Environment
import Text.Printf
import Network.Socket (withSocketsDo)
import Data.Maybe (listToMaybe, isJust)

import Happstack.Server

import Text.Blaze.Html5 ((!), toHtml, docTypeHtml, Html)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

main :: IO ()
main = withSocketsDo $ do

   -- Loading platform
   pf <- loadPlatform defaultConfig {
      filterOpenCLDevices = fmap (notElem CL.CL_DEVICE_TYPE_CPU) . getDeviceType'
   }

   getArgs >>= \case
      []  -> server (nullConf { port = 8000}) pf
      [p] -> server (nullConf { port = read p}) pf
      _   -> putStrLn =<< (printf "Usage: %s [PORT]" <$> getProgName)


server :: Conf -> V.Host -> IO ()
server conf pf = do
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   simpleHTTP conf $ msum
      [ 
      
        -- CSS 
        dir "css" $ dir "style.css" $ do
         cssPath <- lift $ getDataFileName "data/web/css/style.css"
         serveFile (asContentType "text/css") cssPath

        -- Show platform information
      , dir "platform" $ dir "memory" $ path $ \uid -> showMemory pf uid

        -- Show platform information
      , dir "platform" $ showHost pf

        -- Show welcome screen
      , nullDir >> (ok . toResponse . appTemplate pf "Welcome" $ showWelcome)
      ]

-- | Template of all pages
appTemplate :: V.Host -> String -> Html -> Html
appTemplate _ title bdy = docTypeHtml $ do
   H.head $ do
      H.title "ViperVM Web Interface"
      H.meta ! A.httpEquiv "Content-Type"
             ! A.content "text/html;charset=utf-8"
      H.link ! A.rel "stylesheet" 
             ! A.type_ "text/css" 
             ! A.href "/css/style.css"
   H.body $ do
      H.div (toHtml $ "ViperVM " ++ showVersion version ++ " / " ++ title)
         ! A.class_ "headtitle"
      bdy

-- | Welcoming screen
showWelcome :: Html
showWelcome = do
   H.p "Welcome to ViperVM Web Interface"
   H.a "Platform information" ! A.href "/platform"

-- | Show the host
showHost :: V.Host -> ServerPartT IO Response
showHost pf = do

   (mems,procs,nets) <- lift . atomically $ do
      let extractMem xs x = return (x:xs)
      mems <- reverse <$> foldMemories pf [] extractMem
      procs <- Set.toList . Set.unions <$> mapM (readTVar . memoryProcs) mems
      nets <- Set.toList . Set.unions <$> mapM (readTVar . memoryNetworks) mems
      return (mems,procs,nets)

   ok . toResponse . appTemplate pf "Platform" $ do
      H.h2 "Memories"
      H.ul $ forM_ mems $ \mem -> H.li $ do
         H.a (toHtml $ memoryInfo mem)
            ! A.href (H.toValue $ "/platform/memory/" ++ memoryUID mem)

      H.h2 "Processors"
      H.ul $ forM_ procs $ \p -> do
         H.li . toHtml $ procInfo p

      H.h2 "Networks"
      H.ul $ forM_ nets $ \p -> do
         H.li . toHtml $ networkInfo p

-- | Show a memory
showMemory :: V.Host -> String -> ServerPartT IO Response
showMemory pf uid = do
   let extractMem xs x = return (x:xs)
   mems <- lift $ atomically (foldMemories pf [] extractMem)

   let mem = listToMaybe [x | x <- mems, memoryUID x == uid]

   -- check that the memory with the given identifier exists
   guard (isJust mem)

   let Just m = mem
   (nbuffers,bufferSizes) <- lift $ atomically $ do
      n <- memoryBufferCount m
      szs <- fmap bufferSize <$> (TSet.toList $ memoryBuffers m)
      return (n,szs)
   
   ok . toResponse . appTemplate pf ("Memory - " ++ uid) $ do
      H.h2 (toHtml $ "Memory" ++ uid)

      H.ul $ H.li $ toHtml (memoryInfo m)

      H.h2 (toHtml (printf "Buffers (%d)" nbuffers :: String))

      H.ul $ forM_ bufferSizes $ \sz -> do
         let sizeMB = (fromIntegral sz / (1024.0 * 1024.0) :: Float)
         H.li . toHtml $ (printf "Buffer - %f MB" sizeMB :: String)
