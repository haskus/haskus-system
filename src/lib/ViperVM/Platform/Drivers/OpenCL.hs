-- | OpenCL driver
module ViperVM.Platform.Drivers.OpenCL
   ( Memory(..)
   , Buffer(..)
   , Network(..)
   , Proc(..)
   , allocateBuffer
   , releaseBuffer
   , transferHostToDevice
   , transferDeviceToHost
   , clMemUID
   , clBufferUID
   )
where

import Data.Word (Word64)
import Data.Ord (comparing)
import Foreign.Ptr (Ptr,plusPtr)
import Text.Printf

import ViperVM.Arch.Common.Endianness
import ViperVM.Arch.Common.Errors
import ViperVM.Platform.Memory.Region
import ViperVM.Platform.TransferResult
import qualified ViperVM.Arch.OpenCL.All as CL

data Memory = Memory
   { clMemLibrary    :: CL.Library
   , clMemDevice     :: CL.Device
   , clMemContext    :: CL.Context
   , clMemEndianness :: Endianness
   , clMemSize       :: Word64
   }

instance Eq Memory where
   (==) a b = clMemDevice a == clMemDevice b

instance Ord Memory where
   compare = comparing clMemDevice

data Buffer = Buffer
   { clBufferDevice  :: CL.Device
   , clBufferContext :: CL.Context
   , clBufferPeer    :: CL.Mem
   } deriving (Eq)

instance Ord Buffer where
   compare = comparing clBufferPeer

data Network = Network
   { clLinkDevice    :: CL.Device
   , clLinkContext   :: CL.Context
   , clLinkQueue     :: CL.CommandQueue
   }

instance Eq Network where
   (==) a b = clLinkDevice a == clLinkDevice b

instance Ord Network where
   compare = comparing clLinkDevice

data Proc = Proc 
   { clProcDevice    :: CL.Device
   , clProcContext   :: CL.Context
   }

instance Eq Proc where
   (==) a b = clProcDevice a == clProcDevice b

instance Ord Proc where
   compare = comparing clProcDevice

-- | Unique memory ID
clMemUID :: Memory -> String
clMemUID mem = printf "OpenCL Memory %s" (show . CL.unwrap . clMemDevice $ mem)

-- | Unique buffer ID
clBufferUID :: Buffer -> String
clBufferUID buf = printf "OpenCL Buffer %s %s" (show . CL.unwrap . clBufferDevice $ buf) (show . CL.unwrap . clBufferPeer $ buf)


-- | Allocate a buffer in OpenCL memory
allocateBuffer :: Word64 -> Memory -> IO (Either AllocError Buffer)
allocateBuffer size mem = do
   let 
      ctx = clMemContext mem
      dev = clMemDevice mem
      flags = []
   buf <- CL.createBuffer dev ctx flags (fromIntegral size)
   
   return $ case buf of
      Right m -> Right (Buffer dev ctx m)
      Left _ -> Left AllocUnknownError -- FIXME: return appropriate error

-- | Release a buffer in OpenCL memory
releaseBuffer :: Memory -> Buffer -> IO ()
releaseBuffer _ buf = CL.release (clBufferPeer buf)

type CLTransfer = CL.CommandQueue -> CL.Mem -> Bool -> Word64 -> Word64 -> Ptr () -> [CL.Event] -> IO (Either CL.CLError CL.Event)

transfer :: Network -> CLTransfer -> (Ptr (), Region) -> (Buffer,Region) -> IO TransferResult
transfer net f (hostPtr,Region off1 sh1) (buffer,Region off2 sh2) = do

   let
      cq = clLinkQueue net
      mem = clBufferPeer buffer

      clTransfer (Left _) = return (TransferError ErrTransferUnknown)
      clTransfer (Right ev) = CL.waitForEvents [ev] >> return TransferSuccess
      
      -- FIXME: unsafe coercion from CSize to Int
      ptr = hostPtr `plusPtr` fromIntegral off1

   case (sh1,sh2) of

      (Shape1D sz1, Shape1D sz2) 
         | sz1 == sz2 -> clTransfer =<< f cq mem True off2 sz1 ptr []
         | otherwise  -> return (TransferError ErrTransferIncompatibleRegions)

      -- TODO: 2D transfers



transferHostToDevice :: Network -> (Ptr (),Region) -> (Buffer,Region) -> IO TransferResult
transferHostToDevice net = transfer net CL.enqueueWriteBuffer 

transferDeviceToHost :: Network -> (Buffer,Region) -> (Ptr (),Region) -> IO TransferResult
transferDeviceToHost net = flip (transfer net CL.enqueueReadBuffer)
