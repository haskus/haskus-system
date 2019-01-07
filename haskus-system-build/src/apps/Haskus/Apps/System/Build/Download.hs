{-# LANGUAGE FlexibleContexts #-}

module Haskus.Apps.System.Build.Download
   ( download
   )
where

import Network.HTTP.Simple
import Conduit

download ::
   ( MonadUnliftIO m
   , MonadThrow m
   ) => String -> FilePath -> m ()
download url filepath = do
   req <- parseRequest url
   runResourceT $ httpSink req $ const (sinkFile filepath)
