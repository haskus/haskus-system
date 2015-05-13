-- | OpenCL memory object (buffer, image) module
module ViperVM.Arch.OpenCL.Mem
   ( Mem(..)
   , MemFlag(..)
   , MemFlags
   , MapFlag(..)
   , MapFlags
   , createBuffer
   , createImage2D
   , createImage3D
   , enqueueReadBuffer
   , enqueueWriteBuffer
   , enqueueCopyBuffer
   )
where

import qualified ViperVM.Utils.BitSet as BitSet
import ViperVM.Utils.BitSet (EnumBitSet, BitSet)

import ViperVM.Arch.OpenCL.Types
import ViperVM.Arch.OpenCL.Entity
import ViperVM.Arch.OpenCL.Library
import ViperVM.Arch.OpenCL.Error
import ViperVM.Arch.OpenCL.Event
import ViperVM.Arch.OpenCL.CommandQueue
import ViperVM.Arch.OpenCL.Context
import ViperVM.Arch.OpenCL.Device
import ViperVM.Arch.OpenCL.Bindings

import Foreign.C.Types (CSize)
import Control.Monad (void)
import Data.Ord (comparing)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign (allocaArray,pokeArray)
import Foreign.Storable (peek)

-- | Memory object (buffer, image)
data Mem = Mem Library Mem_ deriving (Eq)

instance Ord Mem where
   compare = comparing unwrap

instance Entity Mem where 
   unwrap (Mem _ x) = x
   cllib (Mem l _) = l
   retain = retainMem
   release = releaseMem

-- | Memory object flags
data MemFlag
   = CL_MEM_READ_WRITE        -- 1
   | CL_MEM_WRITE_ONLY        -- 2
   | CL_MEM_READ_ONLY         -- 4
   | CL_MEM_USE_HOST_PTR      -- 8
   | CL_MEM_ALLOC_HOST_PTR    -- 16
   | CL_MEM_COPY_HOST_PTR     -- 32
   | CL_MEM_RESERVED          -- 64
   | CL_MEM_HOST_WRITE_ONLY   -- 128
   | CL_MEM_HOST_READ_ONLY    -- 256
   | CL_MEM_HOST_NO_ACCESS    -- 512
   deriving (Show, Bounded, Eq, Ord, Enum)

instance EnumBitSet MemFlag

type MemFlags = BitSet Word64 MemFlag

-- | Memory object mapping flags
data MapFlag
   = CL_MAP_READ
   | CL_MAP_WRITE
   | CL_MAP_WRITE_INVALIDATE_REGION
   deriving (Show, Bounded, Eq, Ord, Enum)

instance EnumBitSet MapFlag

type MapFlags = BitSet Word64 MapFlag

-- | Memory object type
data MemObjectType
   = CL_MEM_OBJECT_BUFFER
   | CL_MEM_OBJECT_IMAGE2D
   | CL_MEM_OBJECT_IMAGE3D
   | CL_MEM_OBJECT_IMAGE2D_ARRAY
   | CL_MEM_OBJECT_IMAGE1D
   | CL_MEM_OBJECT_IMAGE1D_ARRAY
   | CL_MEM_OBJECT_IMAGE1D_BUFFER
   deriving (Show, Enum)

instance CLConstant MemObjectType where
   toCL x = fromIntegral (fromEnum x + 0x10F0)
   fromCL x = toEnum (fromIntegral x - 0x10F0)


-- | Create a buffer
createBuffer :: Device -> Context -> MemFlags -> CSize -> CLRet Mem
createBuffer _ ctx flags size = do
   let lib = cllib ctx
   mem <- fmap (Mem lib) <$> wrapPError (rawClCreateBuffer lib (unwrap ctx) (BitSet.toBits flags) size nullPtr)
   --FIXME: ensure buffer is allocated 
   --  use clEnqueueMigrateMemObjects if available (OpenCL 1.1 or 1.2?)
   --  perform a dummy operation on the buffer (OpenCL 1.0)
   return mem

-- | Create 2D image
createImage2D :: Context -> MemFlags -> CLImageFormat_p -> CSize -> CSize -> CSize -> Ptr () -> CLRet Mem
createImage2D ctx flags imgFormat width height rowPitch hostPtr =
   fmap (Mem lib) <$> wrapPError (rawClCreateImage2D lib (unwrap ctx) (BitSet.toBits flags) imgFormat width height rowPitch hostPtr)
   where lib = cllib ctx

-- | Create 3D image
createImage3D :: Context -> MemFlags -> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> CLRet Mem
createImage3D ctx flags imgFormat width height depth rowPitch slicePitch hostPtr =
   fmap (Mem lib) <$> wrapPError (rawClCreateImage3D lib (unwrap ctx) (BitSet.toBits flags) imgFormat width height depth rowPitch slicePitch hostPtr)
   where lib = cllib ctx

-- | Release a mem object
releaseMem :: Mem -> IO ()
releaseMem mem = void (rawClReleaseMemObject (cllib mem) (unwrap mem))

-- | Retain a mem object
retainMem :: Mem -> IO ()
retainMem mem = void (rawClRetainMemObject (cllib mem) (unwrap mem))

-- | Helper function to enqueue commands
enqueue :: Library -> (CLuint -> Ptr Event_ -> Ptr Event_ -> IO CLint) -> [Event] -> CLRet Event
enqueue lib f [] = alloca $ \event -> whenSuccess (f 0 nullPtr event) (Event lib <$> peek event)
enqueue lib f events = allocaArray nevents $ \pevents -> do
  pokeArray pevents (fmap unwrap events)
  alloca $ \event -> whenSuccess (f cnevents pevents event) (Event lib <$> peek event)
    where
      nevents = length events
      cnevents = fromIntegral nevents

-- | Transfer from device to host
enqueueReadBuffer :: CommandQueue -> Mem -> Bool -> Word64 -> Word64 -> Ptr () -> [Event] -> CLRet Event
enqueueReadBuffer cq mem blocking off size ptr = 
   enqueue lib (rawClEnqueueReadBuffer lib (unwrap cq) (unwrap mem) (fromBool blocking) (fromIntegral off) (fromIntegral size) ptr)
   where lib = cllib cq

-- | Transfer from host to device
enqueueWriteBuffer :: CommandQueue -> Mem -> Bool -> Word64 -> Word64 -> Ptr () -> [Event] -> CLRet Event
enqueueWriteBuffer cq mem blocking off size ptr = 
   enqueue lib (rawClEnqueueWriteBuffer lib (unwrap cq) (unwrap mem) (fromBool blocking) (fromIntegral off) (fromIntegral size) ptr)
   where lib = cllib cq

-- | Copy from one buffer to another
enqueueCopyBuffer :: CommandQueue -> Mem -> Mem -> Word64 -> Word64 -> Word64 -> [Event] -> CLRet Event
enqueueCopyBuffer cq src dst srcOffset dstOffset sz =
   enqueue lib (rawClEnqueueCopyBuffer lib (unwrap cq) (unwrap src) (unwrap dst) (fromIntegral srcOffset) (fromIntegral dstOffset) (fromIntegral sz))
   where lib = cllib cq

