{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import ViperVM.Platform.Host as V
import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Loading
import ViperVM.Platform.Config
import ViperVM.Platform.Topology as V
import ViperVM.Arch.OpenCL.All as CL

import Paths_ViperVM
import Data.Version

import Control.Concurrent.STM
import qualified Data.Set as Set

import Control.Applicative ((<$>))
import Control.Monad (msum, forM_)
import Control.Monad.Trans.Class (lift)
import System.Environment
import Text.Printf
import Network.Socket (withSocketsDo)

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
      [ dir "css" $ dir "style.css" $ do
         cssPath <- lift $ getDataFileName "data/web/css/style.css"
         serveFile (asContentType "text/css") cssPath

        -- Show platform information
      , dir "platform" $ showPlatform pf

        -- Show welcome screen
      , nullDir >> (ok . toResponse . appTemplate pf "Welcome" $ showWelcome)
      ]

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

showWelcome :: Html
showWelcome = do
   H.p "Welcome to ViperVM Web Interface"
   H.a "Platform information" ! A.href "/platform"

showPlatform :: V.Host -> ServerPartT IO Response
showPlatform pf = do

   (mems,procs,nets) <- lift . atomically $ do
      let extractMem xs x = return (x:xs)
      mems <- reverse <$> foldMemories pf [] extractMem
      procs <- Set.toList . Set.unions <$> mapM (readTVar . memoryProcs) mems
      nets <- Set.toList . Set.unions <$> mapM (readTVar . memoryNetworks) mems
      return (mems,procs,nets)

   ok . toResponse . appTemplate pf "Platform" $ do
      H.h2 "Memories"
      H.ul $ forM_ mems $ \mem -> do
         H.li . toHtml $ memoryInfo mem

      H.h2 "Processors"
      H.ul $ forM_ procs $ \p -> do
         H.li . toHtml $ procInfo p

      H.h2 "Networks"
      H.ul $ forM_ nets $ \p -> do
         H.li . toHtml $ networkInfo p
