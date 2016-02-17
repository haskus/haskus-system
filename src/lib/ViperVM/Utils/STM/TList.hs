-- | Transactional list
module ViperVM.Utils.STM.TList
   ( TList
   , TNode
   , empty
   , singleton
   , null
   , length
   , first
   , last
   , prev
   , next
   , value
   , delete
   , append
   , append_
   , prepend
   , prepend_
   , insertBefore
   , insertAfter
   , toList
   , fromList
   )
where

import Prelude hiding (null,length,last)

import Control.Concurrent.STM
import Data.Foldable (forM_)
import Control.Monad (void)

import qualified Data.STM.LinkedList as LL

-- | A transactional list
type TList a = LL.LinkedList a

-- | A node in the list
type TNode a = LL.Node a

-- | Get value associated with a node
value :: TNode a -> a
value = LL.value

-- | Create an empty list
empty :: STM (TList e)
empty = LL.empty

-- | Create a singleton list
singleton :: e -> STM (TList e)
singleton e = do
   m <- empty
   void $ append e m
   return m

-- | Indicate if the list is empty
null :: TList e -> STM Bool
null = LL.null

-- | Count the number of elements in the list (0(n))
length :: TList e -> STM Int
length = LL.length

-- | Get the first element if any
first :: TList e -> STM (Maybe (TNode e))
first = LL.start

-- | Get the last element if any
last :: TList e -> STM (Maybe (TNode e))
last = LL.end

-- | Get the previous element if any
prev :: TNode a -> STM (Maybe (TNode a))
prev = LL.prev

-- | Get the next element if any
next :: TNode a -> STM (Maybe (TNode a))
next = LL.next

-- | Delete a element of the list
delete :: TNode a -> STM ()
delete = LL.delete

-- | Append an element to the list
append :: a -> TList a -> STM (TNode a)
append = LL.append

-- | Append an element to the list
append_ :: a -> TList a -> STM ()
append_ a = void . append a

-- | Prepend an element to the list
prepend :: a -> TList a -> STM (TNode a)
prepend = LL.prepend

-- | Prepend an element to the list
prepend_ :: a -> TList a -> STM ()
prepend_ a = void . prepend a

-- | Insert an element before another
insertBefore :: a -> TNode a -> STM (TNode a)
insertBefore = LL.insertBefore

-- | Insert an element after another
insertAfter :: a -> TNode a -> STM (TNode a)
insertAfter = LL.insertAfter

-- | Convert into a list
toList :: TList a -> STM [a]
toList = LL.toList

-- | Create from a list
fromList :: [e] -> STM (TList e)
fromList xs = do
   s <- empty
   forM_ xs (`append` s)
   return s
