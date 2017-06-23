## Version 0.8 (next)

## Version 0.7 (2017-06-23)

* System
   * (Terminal) Use writeMany syscall to reduce the number of write syscalls
   * (Graphics) Better support for Planes
   * (Graphics) Add a configuration monad (to support atomic configuration in
     the future)
   * (Graphics) Add page-flip target support
   * (Graphics) Add non-atomic property support
   * (Graphics) Detect and report the flipped framebuffer
   * (Sound) Fix field sizes
   * (Syscalls) Replace the type-level syscall table with a TH generated table
   * (PCI) Add PCI device identifier table
* Utils
   * New flow operators (>.~!!> and all the MaybeCatchable variants)
   * Preliminary continuation stuff
* Various
   * Package renamed from ViperVM to haskus-system
   * Package split into haskus-utils, haskus-binary and haskus-system
   * Switch to GHC 8.0
   * Most uses of Proxy have been replaced with type applications
   * Tests now use Tasty
   * Removed useless dependencies
   * Provide the haskus-system-build tool (in a separate package)
   * Add Travis CI support


## Version 0.6 (2016-11-13)

* Switch to GHC 8.0.1
   * Use TypeApplications a lot
* Switch to BSD-3-clause license
* New website
* Linux
   * (Devices) Device manager with device index (by subsystem) and notifications
   * (Graphics) Object property support (get only for now)
   * (Graphics) Add simple rendering engine suporting multi-buffering
   * (Graphics) Use Rasterific text renderer (+ upstream patch to embed fonts)
   * (System) Make logging in Sys monad support fork
   * (Input) Support bundles of input events (separated with sync events)
   * (Input) Better event type
   * (Input) List supported events
   * (syscalls) use a type-level table
   * (syscalls) support all x86-64 syscalls
   * (syscalls) support both PrimOp and FFI safe methods
   * Device data type: major + minor + char/block
* Format
   * (ELF) Decode GHC Z-String symbol names
* Binary
   * (Unums) Preliminary support for Unums 2.0
   * (Ptr) Use a PtrLike class
   * (Buffer) Enhance primitives
   * (Enum) Build enums generically with 0(1) convertion using statically
     computed tables (cf Embed bytes)
   * (Storable) Use our own Storable class (with default signatures)
* System
   * Support EAGAIN in output thread
* Apps
   * (x86 instructions viewer) Better HTML rendering for the operands
* Utils
   * STM Tree data structure
   * New flow operators: const variants
   * Add Tuple, List, Maybe, etc. modules
   * (Embed) Embed bytes
   * (Monad) Use MonadIO and MonadInIO to support both Sys and IO monads
* Remove dependency on:
   * HList
   * c-storable-deriving
   * criterion
   * dynamic-linker-template
* Remove legacy modules: OpenCL and Platform
* Many minor fixes
* Some tests added

## Version 0.5 (2016-08-23)

* X86-64 disassembler
   * Add new instructions (e.g., RET2)
   * Various fixes
   * Support text output (Haskus.Apps.Disassembler)
   * Add disassembler for code sections in ELF viewer app
* Linux
   * (Graphics) Fix: correctly handle buffer pitch
   * Add auxiliary vector support
   * Enhance process memory mapping support
   * (Syscalls) Added gettimeofday, settimeofday
* Binary modules
   * Added modules: Word, Bits, Ptr, etc.
   * Vector now uses Buffer
* CPIO
   * Fix CPIO archive generation
* ISO9660 format (ISO files)
   * Work-in-progress

## Version 0.4 (2016-06-10)

* Haskus.Format.Binary
    * Modules to write bindings and to manipulate binary data
* Haskus.Format
    * Support for several formats: CPIO, ELF, DWARF, GZIP
* Haskus.Arch.X86_64
    * instruction list
    * X86 disassembler
    * basic CPUID interface
* Haskus.Arch.X86_64.Linux
    * Low-level Linux syscall wrappers on X86-64
* Haskus.System.Linux.Internals
    * Low-level interface to Linux's subsystems:
        * DRM/KMS
        * Alsa
        * Input
* Haskus.System.Linux
    * interface with Linux common syscalls and subsystems
* Haskus.System
    * High-level more generic system interface used by applications
* Haskus.Utils
    * Variant: type-safe open sum type
    * Flow: generic function composition combinators based on Variants

## Version 0.3 (2014-11-18)

* Platform
    * Support meta-data (data with instances in different memories)

## Version 0.2 (2014-09-10)

* Platform
    * Support data allocation/release in memories
    * Support data transfers over links

## Version 0.1 (2014-02-27)

* Platform
    * Report basic architecture information
        * Memories and processing units
        * Capacities
        * Interconnections (graph)
    * Support OpenCL and Host (without NUMA)
