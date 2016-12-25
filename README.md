# Haskus system

Haskus system is a framework written in Haskell that can be used for system
programming. The long-term aim is to provide a full Haskell user-space
environment on top of the Linux kernel.

Website: http://www.haskus.org/system

# How to build

Use stack commands to build Haskus system:

```bash
$ stack setup
$ stack build
```

Tests:
```bash
$ stack test
```

Benchmarks:
```bash
$ stack bench
```

# Module Overview

## Binary modules

Haskus system handles low-level data structures in memory such as C structs,
unions, enums, bit fields, etc. It doesn't depend on C header files (.h) and
doesn't use preprocessors (cpp2hs, hsc2hs, etc.).

* Haskus.Format.Binary: modules to manipulate binary data and to easily create
  C bindings (see the [documentation](doc/manual/binary.md))

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

* Haskus.Arch.X86_64: Currently only X86-64 is supported [documentation](doc/manual/x86.md)
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

# Programs

Several programs are bundled with Haskus:

#### ELF Web

ElfWeb program can be used to navigate into a ELF binary file. Use your Web
browser to see the result.

```bash
$ stack exec -- ElfWeb -p 8020 ./mybinary &
$ firefox http://localhost:8020
```

#### X86 Web

Show info about the x86 instructions recognized by Haskus system.

```bash
$ stack exec -- X86Web -p 8020 &
$ firefox http://localhost:8020
```

#### GUnzip

Simple decompressor for the GZip format.

```bash
$ tar czf test.tgz # some files...
$ stack exec -- gunzip test.tgz
```

#### udev

Dump kernel system events (i.e. changes into the system tree) on standard output.

```bash
$ stack exec udev
$ # try plugging or unplugging a device (USB stick, etc.)
```
