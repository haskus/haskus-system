Haskus System
=============

:Author: Sylvain Henry

Haskus System is a framework written in Haskell that can be used for system
programming. Fundamentally it is an experiment into providing an integrated
interface leveraging Haskell features (type-safety, STM, etc.) for the whole
system: input, display, sound, network, etc.

It is based directly and only on the Linux kernel:

* it doesn't contain any kernel level code (device driver, etc.)

* it doesn't rely on usual interfaces (e.g., libdrm, libinput, X11, wayland,
  etc.) to communicate with the kernel

Note that it still depends on GHC's RTS dependencies (libc, etc.).

.. contents::


Introduction
------------

A typical system can be roughly split into three parts:

* Kernel: device drivers, virtual memory management, process scheduling,
  etc.

* System: system services and daemons, low-level kernel interfaces, etc.

* Application: end-user applications (web browser, video player, games, etc.)

The first aim of the Haskus System framework is to make it easy to experiment
different approaches at the "system" level (which obviously has an impact on the
"application" level). In particular, we would like to be able to easily revisit
long-standing concepts, such as:

* Services and applications: integration with networks (Internet
  services, private cloud...), with hot-pluggable devices, etc. 

* System infrastructure: application management (distribution,
  installation), code sharing (libraries and linking), file systems,
  security, etc.

* User interface: interaction with the user (input and output).

The second aim is to make system programming more enjoyable and productive by
using a high-level language (Haskell) and by providing a hopefully coherent and
well-documented framework with interfaces that are easy to use.


Overview
--------

Binary modules
~~~~~~~~~~~~~~

Haskus system handles low-level data structures in memory such as C structs,
unions, enums, bit fields, etc. It doesn't depend on C header files (.h) and
doesn't use preprocessors (cpp2hs, hsc2hs, etc.).

* Haskus.Format.Binary: modules to manipulate binary data and to easily
  create C bindings (see the `documentation`__)

__ manual/binary.md

Interface with the Linux kernel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Haskus system provides foreign primops to call Linux system calls from Haskell
code without going through the libc. In addition to basic system calls, it
provides wrappers for some Linux subsystems/features accessible through
multiplexing syscalls (e.g., ioctl) or through specific file systems (e.g.,
procfs, sysfs).

* Haskus.Arch.Linux: system calls and low-level interfaces
* Haskus.Arch.Linux.Input: input subsystem
* Haskus.Arch.Linux.Graphics: kms/drm subsystem

Formats
~~~~~~~

Haskus system provides support for some file formats (e.g., ELF, DWARF, CPIO)
and some file system formats (e.g., ISO9660). These can be used to interact
with Linux (e.g., to look up for functions in the vDSO ELF image), to build
initramfs images or bootable disk images, etc.

* Haskus.Format.Compression: some compression algorithms and containers
* Haskus.Format.CPIO: CPIO archive format
* Haskus.Format.Elf: ELF object format
* Haskus.Format.Dwarf: DWARF debugging information format

Architectures
~~~~~~~~~~~~~

Haskus system provides architecture specific modules (currently only for
x86-64), in particular the thin architecture specific layer to call Linux
system calls. Additionally, Haskus has a dictionnary of x86 instructions; it is
currently used to implement a disassembler and could be used to implement
assemblers, analyzers, emulators, etc. A wrapper for the x86's cpuid
instruction is also provided.

* Haskus.Arch.X86_64: Currently only X86-64 is supported (`documentation`__)

  * Haskus.Arch.X86_64.ISA: instruction set architecture
  * Haskus.Arch.X86_64.Disassembler
  * Haskus.Arch.X86_64.Linux: arch-specific Linux interface (syscalls)
  * Haskus.Arch.X86_64.Cpuid: CPUID wrapper

__ manual/x86.md

System interface
~~~~~~~~~~~~~~~~

Haskus system provides modules to interact with the system: input devices,
display devices, etc. These modules are used to easily build a custom system
without dealing directly with the low-level Linux interface. It also provides a
custom monad with common features for system programming (logging, etc.).

* Haskus.System
