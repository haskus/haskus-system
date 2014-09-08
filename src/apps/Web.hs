{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import ViperVM.Platform.Host as V
import ViperVM.Platform.PlatformInfo
import ViperVM.Platform.Loading
import ViperVM.Platform.Config
import ViperVM.Platform.Topology as V
import ViperVM.Arch.OpenCL.All as CL

import Control.Concurrent.STM
import qualified Data.Set as Set
import Data.Set (Set)
-- FIXME: remove unsafe parts
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative ((<$>))
import Control.Monad (msum, guard, forM_, when)
import Control.Monad.Trans.Class (lift)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (intersperse, groupBy)
import Data.Maybe (isJust, fromJust, fromMaybe, listToMaybe)
import Data.Text.Lazy (pack)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Foldable as Fold
import System.Environment
import Text.Printf
import Control.Exception (try)
import Network.HTTP.Base (urlEncode)
import Network.Socket (withSocketsDo)

import Diagrams.Prelude (text, (<>), rect)
import qualified Diagrams.Prelude as Diag
import Diagrams.Core (renderDia)
import Diagrams.Backend.SVG

import Data.Graph.Inductive
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Exception as GV
import qualified Data.GraphViz.Attributes.Complete as GV

import Happstack.Server

import Text.Blaze.Html5 ((!), toHtml, docTypeHtml, toValue, Html, preEscapedToMarkup, preEscapedToValue)
import qualified Text.Blaze.Renderer.Text as BlazeT
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

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
   simpleHTTP conf $ msum [
           -- Show platform information
           dir "platform" $ do
               (mems,procs,nets) <- lift . atomically $ do
                  let extractMem xs x = return (x:xs)
                  mems <- reverse <$> foldMemories pf [] extractMem
                  procs <- Set.toList . Set.unions <$> mapM (readTVar . memoryProcs) mems
                  nets <- Set.toList . Set.unions <$> mapM (readTVar . memoryNetworks) mems
                  return (mems,procs,nets)
               ok . toResponse . appTemplate pf $ showPlatform pf mems procs nets

           -- Show welcome screen
         , nullDir >> (ok . toResponse . appTemplate pf $ showWelcome)
      ]

appTemplate :: V.Host -> Html -> Html
appTemplate pf bdy = docTypeHtml $ do
   H.head $ do
      H.title "ViperVM Web Interface"
      H.meta ! A.httpEquiv "Content-Type"
             ! A.content "text/html;charset=utf-8"
      css
   H.body $ do
      H.h1 "ViperVM Web Interface"
      bdy

showWelcome :: Html
showWelcome = do
   H.p "Welcome to ViperVM Web Interface"
   H.a "Platform information" ! A.href "/platform"

showPlatform :: V.Host -> [Memory] -> [Proc] -> [V.Network] -> Html
showPlatform pf mems procs nets= do
   let 
      showInfo x = putStrLn $ "  - " ++ x

      memoriesStr x 
         | x <= 1    = printf "%d memory found" x
         | otherwise = printf "%d memories found" x
      procsStr x 
         | x <= 1    = printf "%d processor found" x
         | otherwise = printf "%d processors found" x
      netsStr x 
         | x <= 1    = printf "%d network found" x
         | otherwise = printf "%d networks found" x

   H.h2 "Memories"
   H.ul $ forM_ mems $ \mem -> do
      H.li . toHtml $ (unsafePerformIO $ memoryInfo mem)

   H.h2 "Processors"
   H.ul $ forM_ procs $ \p -> do
      H.li . toHtml $ (unsafePerformIO $ procInfo p)

   H.h2 "Networks"
   H.ul $ forM_ nets $ \p -> do
      H.li . toHtml $ (unsafePerformIO $ networkInfo p)

css :: Html
css = do
   H.style $ "\
      \  body {\n\
      \     font-family:monospace;\n\
      \     background-color: white;\n\
      \  }\n\
      \  h1 {\n\
      \     text-align:center;\n\    
      \  }"
