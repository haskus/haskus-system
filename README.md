# ViperVM

ViperVM is a framework *written in Haskell* that can be used for system programming. The long-term aim is to provide a full Haskell user-space environment on top of the Linux kernel.

# How to build

Use stack commands to build ViperVM:

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

ViperVM handles low-level data structures in memory such as C structs, unions, enums, bit fields, etc. It doesn't depend on C header files (.h) and doesn't use preprocessors (cpp2hs, hsc2hs, etc.).

* ViperVM.Format.Binary: modules to manipulate binary data and to easily create
  C bindings (see the [documentation](doc/manual/binary.md))

## Interface with the Linux kernel

ViperVM provides foreign primops to call Linux system calls from Haskell code without going through the libc. In addition to basic system calls, it provides wrappers for some Linux subsystems/features accessible through multiplexing syscalls (e.g., ioctl) or through specific file systems (e.g., procfs, sysfs).

* ViperVM.Arch.Linux: system calls and low-level interfaces
* ViperVM.Arch.Linux.Input: input subsystem
* ViperVM.Arch.Linux.Graphics: kms/drm subsystem

## Formats

ViperVM provides support for some file formats (e.g., ELF, DWARF, CPIO) and some file system formats (e.g., ISO9660). These can be used to interact with Linux (e.g., to look up for functions in the vDSO ELF image), to build initramfs images or bootable disk images, etc.

* ViperVM.Format.Compression: some compression algorithms and containers
* ViperVM.Format.CPIO: CPIO archive format
* ViperVM.Format.Elf: ELF object format
* ViperVM.Format.Dwarf: DWARF debugging information format

## Architectures

ViperVM provides architecture specific modules (currently only for x86-64), in particular the thin architecture specific layer to call Linux system calls. Additionally, ViperVM has a dictionnary of x86 instructions; it is currently used to implement a disassembler and could be used to implement assemblers, analyzers, emulators, etc. A wrapper for the x86's cpuid instruction is also provided.

* ViperVM.Arch.X86_64: Currently only X86-64 is supported [documentation](doc/manual/x86.md)
   * ViperVM.Arch.X86_64.ISA: instruction set architecture
   * ViperVM.Arch.X86_64.Disassembler
   * ViperVM.Arch.X86_64.Linux: arch-specific Linux interface (syscalls)
   * ViperVM.Arch.X86_64.Cpuid: CPUID wrapper


## System interface

ViperVM provides modules to interact with the system: input devices, display devices, etc. These modules are used to easily build a custom system without dealing directly with the low-level Linux interface. It also provide a custom monad with common features for system programming (logging, etc.).

* ViperVM.System

## Runtime system

Historically, ViperVM started as a runtime system for heterogeneous
architectures (GPGPU, etc.). Hence it still provides modules to dynamically load
and use OpenCL libraries as well as some modules to manage distributed memory
(data transfers, etc.). The ultimate goal was to provide an execution
environment using parallel functional programming on heterogeneous
architectures, hence the "VM" (standing for Virtual Machine) in the name of the
project. In its current state, it is far from being useful but maybe this part
of the project could be resurrected in the future.

* ViperVM.Arch.OpenCL: OpenCL related modules
* ViperVM.Platform: abstraction over distributed memory


# Programs

Several programs are bundled with ViperVM:

#### ELF Web

ELFWeb program can be used to navigate into a ELF binary file. Use your Web
browser to see the result.

```bash
$ ELFWeb -p 8020 ./mybinary &
$ firefox http://localhost:8020
```

#### X86 Web

Show info about the x86 instructions recognized by ViperVM.

```bash
$ X86Web -p 8020 &
$ firefox http://localhost:8020
```

#### Platform Web

PlatformWeb gives basic information about the platform (memories, processors,
networks). It can also be used to perform basic operations (e.g. memory
allocation/release) for test purpose.

```bash
$ PlatformWeb -p 8020
$ firefox http://localhost:8020
```

#### GUnzip

Simple decompressor for the GZip format.

```bash
$ tar czf test.tgz # some files...
$ gunzip test.tgz
```


#### udev

Dump kernel system events (i.e. changes into the system tree) on standard output.

```bash
$ udev
$ # try plugging or unplugging a device (USB stick, etc.)
```
