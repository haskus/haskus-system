-- | Transactional list
module ViperVM.STM.TList
   ( TList
   , empty
   , null
   , length
   , first
   , last
   , prev
   , next
   , value
   , delete
   , append
   , prepend
   , insertBefore
   , insertAfter
   , toList
   )
where

import Prelude hiding (null,length,last)

import Control.Concurrent.STM

import qualified Data.STM.LinkedList as LL

type TList a = LL.LinkedList a

type TNode a = LL.Node a

-- | Get value associated with a node
value :: TNode a -> a
value = LL.value

-- | Create an empty list
empty :: STM (TList e)
empty = LL.empty

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

-- | Prepend an element to the list
prepend :: a -> TList a -> STM (TNode a)
prepend = LL.prepend

-- | Insert an element before another
insertBefore :: a -> TNode a -> STM (TNode a)
insertBefore = LL.insertBefore

-- | Insert an element after another
insertAfter :: a -> TNode a -> STM (TNode a)
insertAfter = LL.insertAfter

-- | Convert into a list
toList :: TList a -> STM ([a])
toList = LL.toList
