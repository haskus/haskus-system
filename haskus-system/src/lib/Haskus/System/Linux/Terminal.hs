{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides some functions to use Linux
-- terminals
module Haskus.System.Linux.Terminal
   ( stdin
   , stdout
   , stderr
   )
   where

import Haskus.System.Linux.Handle

-- | Standard input (by convention)
stdin :: Handle
stdin = Handle 0

-- | Standard output (by convention)
stdout :: Handle
stdout = Handle 1

-- | Standard error output (by convention)
stderr :: Handle
stderr = Handle 2
