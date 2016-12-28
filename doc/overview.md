# Module Overview

## Binary modules

Haskus system handles low-level data structures in memory such as C structs,
unions, enums, bit fields, etc. It doesn't depend on C header files (.h) and
doesn't use preprocessors (cpp2hs, hsc2hs, etc.).

* Haskus.Format.Binary: modules to manipulate binary data and to easily create
  C bindings (see the [documentation](../manual/binary.md))

## Interface with the Linux kernel

Haskus system provides foreign primops to call Linux system calls from Haskell
code without going through the libc. In addition to basic system calls, it
provides wrappers for some Linux subsystems/features accessible through
multiplexing syscalls (e.g., ioctl) or through specific file systems (e.g.,
procfs, sysfs).

* Haskus.Arch.Linux: system calls and low-level interfaces
* Haskus.Arch.Linux.Input: input subsystem
* Haskus.Arch.Linux.Graphics: kms/drm subsystem

## Formats

Haskus system provides support for some file formats (e.g., ELF, DWARF, CPIO)
and some file system formats (e.g., ISO9660). These can be used to interact
with Linux (e.g., to look up for functions in the vDSO ELF image), to build
initramfs images or bootable disk images, etc.

* Haskus.Format.Compression: some compression algorithms and containers
* Haskus.Format.CPIO: CPIO archive format
* Haskus.Format.Elf: ELF object format
* Haskus.Format.Dwarf: DWARF debugging information format

## Architectures

Haskus system provides architecture specific modules (currently only for
x86-64), in particular the thin architecture specific layer to call Linux
system calls. Additionally, Haskus has a dictionnary of x86 instructions; it is
currently used to implement a disassembler and could be used to implement
assemblers, analyzers, emulators, etc. A wrapper for the x86's cpuid
instruction is also provided.

* Haskus.Arch.X86_64: Currently only X86-64 is supported [documentation](../manual/x86.md)
   * Haskus.Arch.X86_64.ISA: instruction set architecture
   * Haskus.Arch.X86_64.Disassembler
   * Haskus.Arch.X86_64.Linux: arch-specific Linux interface (syscalls)
   * Haskus.Arch.X86_64.Cpuid: CPUID wrapper


## System interface

Haskus system provides modules to interact with the system: input devices,
display devices, etc. These modules are used to easily build a custom system
without dealing directly with the low-level Linux interface. It also provides a
custom monad with common features for system programming (logging, etc.).

* Haskus.System
