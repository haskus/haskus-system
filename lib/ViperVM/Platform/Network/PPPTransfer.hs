module ViperVM.Platform.Network.PPPTransfer (
   transferRegion
) where

import Foreign.Ptr (plusPtr)

import ViperVM.Platform.Types
import ViperVM.MMU.Region
import qualified ViperVM.Platform.OpenCL as CL

-- | Perform a synchronous region transfer
transferRegion :: Network -> Buffer -> Region -> Buffer -> Region -> IO (Maybe TransferError)
transferRegion link@(PPPLink {}) bufIn regIn bufOut regOut = do
   let
      bufPeerIn = bufferPeer bufIn
      bufPeerOut = bufferPeer bufOut
      lnkPeer = pppLinkPeer link
      clTransfer (Left _) = return (Just ErrTransferUnknown)
      clTransfer (Right ev) = CL.waitForEvents [ev] >> return Nothing

      -- 1D transfers
      transfer1D off1 off2 sz = case (lnkPeer,bufPeerIn,bufPeerOut) of

         -- OpenCL 1D CL -> Host
         (lnk@(OpenCLLink {}), OpenCLBuffer _ _ mem, HostBuffer ptr) -> do
            let ptr2 = ptr `plusPtr` fromIntegral off2 -- TODO: unsafe coercion from CSize to Int
                cq = clLinkQueue lnk
            clTransfer =<< CL.enqueueReadBuffer cq mem True off1 sz ptr2 []

         -- OpenCL 1D Host -> CL
         (lnk@(OpenCLLink {}), HostBuffer ptr, OpenCLBuffer _ _ mem) -> do
            let ptr2 = ptr `plusPtr` fromIntegral off1 -- TODO: unsafe coercion from CSize to Int
                cq = clLinkQueue lnk
            clTransfer =<< CL.enqueueWriteBuffer cq mem True off2 sz ptr2 []

         _ -> return (Just ErrTransferInvalid)

      -- 2D transfers
      transfer2D = undefined

   -- TODO: check that link interconnects buffers

   case (regIn,regOut) of
      (Region1D off1 sz1, Region1D off2 sz2)
         | sz1 == sz2 -> transfer1D (fromIntegral off1) (fromIntegral off2) (fromIntegral sz1)
      (Region2D _ n1 sz1 _, Region2D _ n2 sz2 _)
         | n1 == n2 && sz1 == sz2 -> transfer2D regIn regOut
      _ -> return (Just ErrTransferIncompatibleRegions)


