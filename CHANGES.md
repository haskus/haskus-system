## Version 0.6 (next)

* Linux
   * (Devices) Device manager with device index (by subsystem) and notifications
   * (Graphics) Object property support (get only for now)
   * (System) Make logging in Sys monad support fork
   * (syscalls) readlink(at)
* Binary
   * Preliminary support for Unums 2.0
* Apps
   * (x86 instructions viewer) Better HTML rendering for the operands

## Version 0.5 (2016-08-23)

* X86-64 disassembler
   * Add new instructions (e.g., RET2)
   * Various fixes
   * Support text output (ViperVM.Apps.Disassembler)
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

* ViperVM.Format.Binary
    * Modules to write bindings and to manipulate binary data
* ViperVM.Format
    * Support for several formats: CPIO, ELF, DWARF, GZIP
* ViperVM.Arch.X86_64
    * instruction list
    * X86 disassembler
    * basic CPUID interface
* ViperVM.Arch.X86_64.Linux
    * Low-level Linux syscall wrappers on X86-64
* ViperVM.Arch.Linux.Internals
    * Low-level interface to Linux's subsystems:
        * DRM/KMS
        * Alsa
        * Input
* ViperVM.Arch.Linux
    * interface with Linux common syscalls and subsystems
* ViperVM.System
    * High-level more generic system interface used by applications
* ViperVM.Utils
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
