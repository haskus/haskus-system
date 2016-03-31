{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

-- | A binary reader monad
--
-- Rely on the MultiState monad to allow concurrent Readers/Writers.
module ViperVM.Format.Binary.Reader
   ( Reader
   , ReaderM
   , binReadT
   , binRead
   , binSkipT
   , binSkip
   , binRemainingT
   , binRemaining
   )
where

import ViperVM.Format.Binary.Buffer
import ViperVM.Utils.MultiState
import ViperVM.Utils.HArray

import Data.Proxy
import Data.Word
import Foreign.Storable

-- | The phantom type `a` is used to distinguish between several readers
newtype Reader a = Reader Buffer

type ReaderM r s =
   ( HArrayIndexT (Reader r) s)

-- | Read an element from the specified reader
binReadT :: forall r m s a. (Monad m, ReaderM r s, Storable a) => Proxy r -> MStateT s m a
binReadT _ = do
   Reader b <- mGet :: MStateT s m (Reader r)
   let (b',a) = bufferRead b
   mSet (Reader b' :: Reader r)
   return a


-- | Read an element from a predefined `Reader ()`
binRead :: forall m s a. (Monad m, ReaderM () s, Storable a) => MStateT s m a
binRead = binReadT (Proxy :: Proxy ()) 

-- | Skip the specified number of bytes
binSkipT :: forall r m s. (Monad m, ReaderM r s) => Proxy r -> Word64 -> MStateT s m ()
binSkipT _ n = do
   Reader b <- mGet :: MStateT s m (Reader r)
   let b' = bufferDrop n b
   mSet (Reader b' :: Reader r)

-- | Skip the specified number of bytes
binSkip :: forall m s. (Monad m, ReaderM () s) => Word64 -> MStateT s m ()
binSkip = binSkipT (Proxy :: Proxy ())

-- | Get the remaining number of bytes
binRemainingT :: forall r m s. (Monad m, ReaderM r s) => Proxy r -> MStateT s m Word64
binRemainingT _ = do
   Reader b <- mGet :: MStateT s m (Reader r)
   return (bufferSize b)

-- | Get the remaining number of bytes
binRemaining :: forall m s. (Monad m, ReaderM () s) => MStateT s m Word64
binRemaining = binRemainingT (Proxy :: Proxy ())
