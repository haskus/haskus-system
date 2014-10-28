-- | Data with several instances
module ViperVM.Platform.Memory.MultiData
   ( MultiData(..)
   , DataInstance(instanceData,instanceRepr)
   , new
   , addInstance
   , removeInstance
   )
where

import Control.Concurrent.STM
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (traverse_)

import ViperVM.Platform.Types (Data)
import ViperVM.STM.TList as TList

-- | A data with possibly more than one instance
--
-- * parameterized with p
-- * stored in different locations and with potentially different representation r
data MultiData p r s = MultiData
   { mdParameters :: p                      -- ^ Parameters of the data
   , mdInstances  :: TList (DataInstance r) -- ^ Data instances
   , mdSources    :: TList s                -- ^ Sources (data from which we can obtain this one using s)
   , mdTargets    :: TList s                -- ^ Targets (data that can/must use our data to create theirs)
   }

-- | Data instance
data DataInstance r = DataInstance
   { instanceData :: Data     -- ^ The data
   , instanceRepr :: r        -- ^ Representation attributes
   , instanceNode :: TMVar (TList.TNode (DataInstance r)) -- ^ Node in the instance list
   }

-- | Create a new MultiData
new :: p -> STM (MultiData p r s)
new p = MultiData p <$> TList.empty <*> TList.empty <*> TList.empty


-- | Add an instance
addInstance :: MultiData p r s -> r -> Data -> STM (DataInstance r)
addInstance md r d = do
   di <- DataInstance d r <$> newEmptyTMVar
   node <- TList.append di (mdInstances md)
   putTMVar (instanceNode di) node
   return di

-- | Detach an instance from its multi-data
removeInstance :: DataInstance r -> STM ()
removeInstance di = do
   node <- tryTakeTMVar (instanceNode di)
   traverse_ TList.delete node
