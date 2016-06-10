## Version 0.4

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


## Version 0.3

* Platform
    * Support meta-data (data with instances in different memories)

## Version 0.2

* Platform
    * Support data allocation/release in memories
    * Support data transfers over links

## Version 0.1

* Platform
    * Report basic architecture information
        * Memories and processing units
        * Capacities
        * Interconnections (graph)
    * Support OpenCL and Host (without NUMA)
