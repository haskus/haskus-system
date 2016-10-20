-- | Data with several instances
module ViperVM.Platform.Memory.Object
   ( Object(..)
   , DataInstance(instanceData,instanceRepr)
   , MultiDatable(..)
   , new
   , addInstance
   , removeInstance
   )
where

import Control.Concurrent.STM

import ViperVM.Platform.Types (Data,MultiData,MultiData_(..))
import ViperVM.Utils.STM.TList as TList
import ViperVM.Utils.Flow

-- | A data with possibly more than one instance
--
-- * parameterized with p
-- * stored in different locations and with potentially different representation r
data Object p r s = Object
   { objectParameters :: p                      -- ^ Parameters of the data
   , objectInstances  :: TList (DataInstance r) -- ^ Data instances
   , objectSources    :: TList s                -- ^ Sources (data from which we can obtain this one using s)
   , objectTargets    :: TList s                -- ^ Targets (data that can/must use our data to create theirs)
   }

-- | Data instance
data DataInstance r = DataInstance
   { instanceData :: Data     -- ^ The data
   , instanceRepr :: r        -- ^ Representation attributes
   , instanceNode :: TMVar (TList.TNode (DataInstance r)) -- ^ Node in the instance list
   }

-- | Multi data
class MultiDatable s where
   toMultiData :: s -> STM MultiData

instance MultiDatable s => MultiData_ (Object p r s) where
   mdInstances = fmap (fmap instanceData) . TList.toList . objectInstances
   mdSources = mapM toMultiData <=< (TList.toList . objectSources)
   mdTargets = mapM toMultiData <=< (TList.toList . objectTargets)

-- | Create a new Object
new :: p -> STM (Object p r s)
new p = Object p <$> TList.empty <*> TList.empty <*> TList.empty


-- | Add an instance
addInstance :: Object p r s -> r -> Data -> STM (DataInstance r)
addInstance obj r d = do
   di <- DataInstance d r <$> newEmptyTMVar
   node <- TList.append di (objectInstances obj)
   putTMVar (instanceNode di) node
   return di

-- | Detach an instance from its multi-data
removeInstance :: DataInstance r -> STM ()
removeInstance di = do
   node <- tryTakeTMVar (instanceNode di)
   mapM_ TList.delete node


