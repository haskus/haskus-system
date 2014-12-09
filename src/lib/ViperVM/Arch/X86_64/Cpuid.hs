-- | Get processor information with the CPUID instruction
module ViperVM.Arch.X86_64.Cpuid
   ( procVendor
   , procName
   )
   where


import qualified System.Cpuid as C

-- | Processor vendor
procVendor :: IO String
procVendor = C.vendorString

-- | Processor name
procName :: IO String
procName = C.brandString
