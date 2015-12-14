# ViperVM library

This package provides a panel of tools for system programming, mostly for
x86-64 architecture and Linux for now. From the bottom up:

## Formats

* ViperVM.Format.Binary: modules to manipulate binary data (bits, Word8, wordN,
  ByteString, etc.)
* ViperVM.Format.Compression: some compression algorithms and containers
* ViperVM.Format.CPIO: CPIO archive format
* ViperVM.Format.Elf: ELF object format
* ViperVM.Format.Dwarf: DWARF debugging information format

## Linux system calls

* ViperVM.Arch.Linux: ViperVM provides direct access to Linux system calls
  (*syscalls*) on x86-64 architecture without going through the libc and
  without going through the GHC FFI mechanism by default (so be careful not to
  use blocking system calls).

On top of that, it gives access to Linux subsystems. For now:

* ViperVM.Arch.Linux.Input: events from keyboard, mouse, touchscreen, sound
  card (when a connector is plugged in/plugged out), etc.
* ViperVM.Arch.Linux.Graphics: manage monitors and what is displayed on them

## Runtime System

* ViperVM.Arch.OpenCL: ViperVM started as a runtime system for heterogeneous
  architectures (GPGPU, etc.), hence it provides modules to dynamically load
  and use OpenCL libraries.

* ViperVM.Platform: these modules are an attempt to abstract over memories and
  processors of heterogeneous architectures to automatically handle
  computations and data transfers. The ultimate goal is to provide an execution
  model using parallel functional programming, but we are not there yet.

* ViperVM.Library: these modules are supposed to handle a library of
  computational kernel sources (OpenCL, C, etc.). Ultimately it should store
  compiled kernels to avoid superfluous compilations, provide statistics about
  their execution times, etc.

# Programs

## ELF Web

ELFWeb program can be used to navigate into a ELF binary file. Use your Web
browser to see the result.

```bash
$ ELFWeb -p 8020 ./mybinary &
$ firefox http://localhost:8020
```

## X86 Web

Show info about the x86 instructions recognized by ViperVM.

## Platform Web

PlatformWeb gives basic information about the platform (memories, processors,
networks). It can also be used to perform basic operations (e.g. memory
allocation/release) for test purpose.

```bash
$ PlatformWeb -p 8020
$ firefox http://localhost:8020
```

## GUnzip

Simple decompressor for the GZip format.

```bash
$ tar czf test.tgz # some files...
$ gunzip test.tgz
```


## udev

Dump kernel system events (i.e. changes into the system tree) on standard output.

```bash
$ udev
$ # try plugging or unplugging a device (USB key, etc.)
```
