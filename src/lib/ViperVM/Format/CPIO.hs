{-# LANGUAGE RecordWildCards #-}

-- | Module implementing the CPIO format (used by Linux initramfs)
module ViperVM.Format.CPIO
   ( FileDesc(..)
   , putFile
   , putFiles
   , getFile
   , getFiles
   )
where

import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString,toString)
import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Foldable (forM_)
import Numeric (showHex,readHex)


{- We only consider the "new" CPIO format because the old ones are deprecated.
 - From the man page: http://people.freebsd.org/~kientzle/libarchive/man/cpio.5.txt
 -
 -   The cpio archive format collects any number of files, directories, and
 -   other file system objects (symbolic links, device nodes, etc.) into a
 -   single stream of bytes.
 -
 - General Format
 -   Each file system object in a cpio archive comprises a header record with
 -   basic numeric metadata followed by the full pathname of the entry and the
 -   file data.  The header record stores a series of integer values that gen-
 -   erally follow the fields in struct stat.  (See stat(2) for details.)  The
 -   variants differ primarily in how they store those integers (binary,
 -   octal, or hexadecimal).  The header is followed by the pathname of the
 -   entry (the length of the pathname is stored in the header) and any file
 -   data.  The end of the archive is indicated by a special record with the
 -   pathname ``TRAILER!!!''.
 -
 -  [..]
 -
 - New ASCII Format
 -   The "new" ASCII format uses 8-byte hexadecimal fields for all numbers and
 -   separates device numbers into separate fields for major and minor num-
 -   bers.
 - 
 -    struct cpio_newc_header {
 - 	   char    c_magic[6];
 - 	   char    c_ino[8];
 - 	   char    c_mode[8];
 - 	   char    c_uid[8];
 - 	   char    c_gid[8];
 - 	   char    c_nlink[8];
 - 	   char    c_mtime[8];
 - 	   char    c_filesize[8];
 - 	   char    c_devmajor[8];
 - 	   char    c_devminor[8];
 - 	   char    c_rdevmajor[8];
 - 	   char    c_rdevminor[8];
 - 	   char    c_namesize[8];
 - 	   char    c_check[8];
 -    };
 - 
 -   Except as specified below, the fields here match those specified for the
 -   old binary format above.
 - 
 -   magic   The string ``070701''.
 - 
 -   check   This field is always set to zero by writers and ignored by read-
 -      ers.  See the next section for more details.
 -
 -   namesize Size of the file path (includes the trailing NUL byte)
 - 
 -   The pathname is followed by NUL bytes so that the total size of the fixed
 -   header plus pathname is a multiple of four.  Likewise, the file data is
 -   padded to a multiple of four bytes.  Note that this format supports only
 -   4 gigabyte files (unlike the older ASCII format, which supports 8 giga-
 -   byte files).
 - 
 -   In this format, hardlinked files are handled by setting the filesize to
 -   zero for each entry except the last one that appears in the archive.
 - 
 - New CRC Format
 -   The CRC format is identical to the new ASCII format described in the pre-
 -   vious section except that the magic field is set to ``070702'' and the
 -   check field is set to the sum of all bytes in the file data.  This sum is
 -   computed treating all bytes as unsigned values and using unsigned arith-
 -   metic.  Only the least-significant 32 bits of the sum are stored.
 -}

data FileDesc = FileDesc
   { fileInode       :: Word64
   , fileMode        :: Word64
   , fileUID         :: Word64
   , fileGID         :: Word64
   , fileNLink       :: Word64
   , fileModifTime   :: Word64
   , fileDevMajor    :: Word64
   , fileDevMinor    :: Word64
   , fileRDevMajor   :: Word64
   , fileRDevMinor   :: Word64
   } deriving (Show)

-- | Put a number as a 8-char string padding left with zeros
putNumber :: Word64 -> Put
putNumber x = putByteString bs
   where
      bs = fromString (replicate (8-len) '0' ++ s)
      s = showHex x ""
      len = length s

-- | Read a number stored as a 8-bytes hexadecimal string
getNumber :: Get Word64
getNumber = readHex' . toString <$> getByteString 8
   where
      readHex' n = case readHex n of
         ((num,""):_) -> num
         _            -> error $ "Invalid hexadecimal number: " ++ n


-- | Return the number of padding bytes to pad to 4
pad4 :: Word64 -> Int
pad4 n = case n `mod` 4 of
   0 -> 0
   x -> 4 - fromIntegral x

-- | Put null bytes to pad to 4
putPad4 :: Integral a => a -> Put
putPad4 n = sequence_ (replicate (pad4 $ fromIntegral n) (putWord8 0x00))

-- | Skip padding bytes for padding to 4
skipPad4 :: Word64 -> Get ()
skipPad4 n = skip (pad4 n)

-- | Put a file in the archive
--
-- * path is the path in the archive
putFile :: FileDesc -> String -> ByteString -> Put
putFile (FileDesc {..}) path content = do
   -- Write magic number
   putByteString (fromString "070701")
   -- Put file description
   putNumber fileInode
   putNumber fileMode
   putNumber fileUID
   putNumber fileGID
   putNumber fileNLink
   putNumber fileModifTime
   putNumber (fromIntegral $ BS.length content)
   putNumber fileDevMajor
   putNumber fileDevMinor
   putNumber fileRDevMajor
   putNumber fileRDevMinor
   -- put the length of the UTF8 encoded string, not the Haskell string
   let bspath = fromString path
   putNumber (fromIntegral $ BS.length bspath)
   putNumber 0 -- checksum
   -- Put file name
   putByteString (fromString path)
   putWord8 0x00 -- ending NUL byte
   putPad4 (110 + BS.length bspath + 1)
   -- Put file contents
   putByteString content
   putPad4 (BS.length content)

-- | Put trailer file
putTrailer :: Put
putTrailer = putFile desc "TRAILER!!!" BS.empty
   where
      desc = FileDesc 0 0 0 0 0 0 0 0 0 0

-- | Put files in archive (with archive ending marker)
putFiles :: [(FileDesc,String,ByteString)] -> Put
putFiles files = do
   forM_ files $ \(desc,name,bs) -> putFile desc name bs
   putTrailer

-- | Get a file from the archive
getFile :: Get (FileDesc,String,ByteString)
getFile = do
   -- Read magic number
   magic <- toString <$> getByteString 6
   when (magic /= "070701") $
      error ("File format not supported (invalid magic number: " ++ magic ++ ")")
   -- Read file description
   ino  <- getNumber
   mode <- getNumber
   uid  <- getNumber
   gid  <- getNumber
   nlnk <- getNumber
   mtim <- getNumber
   size <- getNumber
   mad  <- getNumber
   mid  <- getNumber
   mard <- getNumber
   mird <- getNumber
   nameLength <- getNumber -- includes NUL byte
   _ <- getNumber -- read checksum
   -- Read file name
   fileName <- toString <$> getByteString (fromIntegral nameLength - 1)
   skip 1 -- skip \0 byte
   skipPad4 (110 + nameLength)
   -- Read content
   content <- getByteString (fromIntegral size)
   skipPad4 size

   let fd = FileDesc ino mode uid gid nlnk mtim mad mid mard mird
   return (fd, fileName, content)

-- | Get all the files from the archive
getFiles :: Get [(FileDesc,String,ByteString)]
getFiles = rec []
   where 
      rec xs = do
         x@(_,name,_) <- getFile
         case name of
            "TRAILER!!!" -> return (reverse xs)
            _            -> rec (x:xs)
