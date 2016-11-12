{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Various String formats (C string, etc.)
module ViperVM.Format.String
   ( CChar
   , FS.CString
   , withCString
   , withCStringLen
   , castCCharToChar
   , castCharToCChar
   , peekCStringLen
   , peekCString
   -- * Fixed-size CString buffer
   , CStringBuffer
   , fromCStringBuffer
   , toCStringBuffer
   , emptyCStringBuffer
   )
where

import qualified Foreign.C.String as FS
import Foreign.C.Types (CChar(..))

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Vector as Vec
import ViperVM.Utils.Types
import ViperVM.Utils.Monad

-- | Fixed-size buffer containing a CString
newtype CStringBuffer (n :: Nat)
   = CStringBuffer (Vector n Int8)
   deriving (Storable)

instance KnownNat n => Show (CStringBuffer n) where
   show = show . fromCStringBuffer

-- | Convert a CChar into a Char
castCCharToChar :: CChar -> Char
castCCharToChar = FS.castCCharToChar

-- | Convert a Char into a CChar
castCharToCChar :: Char -> CChar
castCharToCChar = FS.castCharToCChar

-- | Peek a CString whose size is known
peekCStringLen :: MonadIO m => Word -> Ptr CChar -> m String
peekCStringLen len p = liftIO (FS.peekCStringLen (p, fromIntegral len))

-- | Peek a CString
peekCString :: MonadIO m => Ptr CChar -> m String
peekCString = liftIO . FS.peekCString

-- | Convert a \0-terminal vector into a string
fromCStringBuffer :: (KnownNat n) => CStringBuffer (n :: Nat) -> String
fromCStringBuffer (CStringBuffer v) = fmap (castCCharToChar . CChar) . takeWhile (/= 0) . Vec.toList $ v

-- | Convert from a String into a \0-terminal vector
toCStringBuffer :: (KnownNat n) => String -> CStringBuffer (n :: Nat)
toCStringBuffer s = CStringBuffer (Vec.fromFilledListZ 0 . fmap (f . castCharToCChar) $ s)
   where
      f (CChar x) = x

-- | Empty string
emptyCStringBuffer :: (KnownNat n) => CStringBuffer (n :: Nat)
emptyCStringBuffer = CStringBuffer (Vec.replicate 0)

-- | Use a String a a null-terminated string
withCString :: MonadInIO m => String -> (Ptr CChar -> m a) -> m a
withCString s = liftWith (FS.withCString s)

-- | Use a String a a null-terminated string
withCStringLen :: MonadInIO m => String -> (Ptr CChar -> Word -> m a) -> m a
withCStringLen s f = liftWith (FS.withCStringLen s) f'
   where
      f' (p, n) = f p (fromIntegral n)
