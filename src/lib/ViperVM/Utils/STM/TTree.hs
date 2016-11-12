-- | STM mutable tree
module ViperVM.Utils.STM.TTree
   ( TTree (..)
   , TTreePath (..)
   , singleton
   , addChild
   , detachChild
   , attachChild
   , treeFollowPath
   )
where

import qualified ViperVM.Utils.STM.TList as TList
import ViperVM.Utils.STM.TList (TList)
import ViperVM.Utils.STM.TEq
import ViperVM.Utils.STM

-- | A STM mutable tree
data TTree k v = TTree
   { treeKey      :: k                        -- ^ Node identifier
   , treeValue    :: v                        -- ^ Node value
   , treeChildren :: TList (TTree k v)        -- ^ Children
   , treeParent   :: TVar (Maybe (TTree k v)) -- ^ Parent
   }

-- | Path in the tree
newtype TTreePath k = TTreePath [k]


-- | Create a singleton node
singleton :: k -> v -> STM (TTree k v)
singleton k v =
   TTree k v
      <$> TList.empty
      <*> newTVar Nothing

-- | Add a child
addChild :: k -> v -> TTree k v -> STM (TTree k v)
addChild k v parent = do
   n <- TTree k v
      <$> TList.empty
      <*> newTVar (Just parent)

   TList.append_ n (treeChildren parent)
   return n

-- | Detach a child
detachChild :: TEq k => TTree k v -> STM ()
detachChild n = do
   
   -- remove child from parent
   let f c = not <$> (treeKey c `teq` treeKey n)
   p <- readTVar (treeParent n)
   mapM_ (TList.filter f . treeChildren) p

   -- remove parent from child
   writeTVar (treeParent n) Nothing

-- | Attach a child a node (detaching it from a previous one if necessary)
attachChild :: TEq k => TTree k v -> TTree k v -> STM ()
attachChild newparent child = do
   -- detach from the previous parent (if any)
   detachChild child

   -- add to newparent's children list
   TList.append_ child (treeChildren newparent)

   -- set newparent
   writeTVar (treeParent child) (Just newparent)

-- | Follow a path from a parent node
treeFollowPath :: TEq k => TTree k v -> TTreePath k -> STM (Maybe (TTree k v))
treeFollowPath p (TTreePath [])     = return (Just p)
treeFollowPath p (TTreePath (x:xs)) = do
   child <- TList.find (\y -> x `teq` treeKey y) (treeChildren p)
   case child of
      Just c  -> treeFollowPath c (TTreePath xs)
      Nothing -> return Nothing
