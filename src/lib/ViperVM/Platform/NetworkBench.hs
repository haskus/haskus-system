module ViperVM.Platform.NetworkBench
   ( BenchResult(..)
   , NetworkBenchResult(..)
   )
where

import Data.Map (Map)
import Data.Word

-- | Result of a transfer bench
data BenchResult
   = BenchFailed           -- ^ Failed transer
   | BenchSuccess Double   -- ^ Successful transfer with duration
   deriving (Eq,Show,Ord)


-- | Set of transfer bench results for different data sizes
data NetworkBenchResult = NetworkBenchResult
   { netBench1D :: Map Word64 BenchResult
   } deriving (Show,Ord,Eq)

