{-# LANGUAGE LambdaCase #-}

module Build.Linux
   ( linuxBuild
   )
where

import System.Process
import System.Exit
import qualified Data.Text as Text
import Control.Monad

import Build.Config

linuxBuild :: LinuxConfig -> FilePath -> IO ()
linuxBuild config path = do
   let shell' s err = do
         (_,_,_,hdl) <- createProcess (shell s) { cwd = Just path }
         waitForProcess hdl >>= \case
            ExitSuccess   -> return ()
            ExitFailure _ -> err

   -- make default configuration for the arch
   shell' "make x86_64_defconfig"
      $ fail "Unable to build Linux default configuration"

   -- enable/disable/module options
   let opts = linuxOptions config
   forM_ (enableOptions opts) $ \opt ->
      shell' ("./scripts/config -e "++ Text.unpack opt)
         $ fail $ "Unable to enable Linux option: " ++ Text.unpack opt
   forM_ (disableOptions opts) $ \opt ->
      shell' ("./scripts/config -d "++ Text.unpack opt)
         $ fail $ "Unable to disable Linux option: " ++ Text.unpack opt
   forM_ (moduleOptions opts) $ \opt ->
      shell' ("./scripts/config -m "++ Text.unpack opt)
         $ fail $ "Unable to modularize Linux option: " ++ Text.unpack opt

   -- fixup config (interactive)
   shell' "make oldconfig"
      $ fail "Unable to adjust Linux configuration"

   -- build
   shell' ("make " ++ Text.unpack (linuxMakeArgs config))
      $ fail "Unable to build Linux"
