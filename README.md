# ViperVM

This package provides a panel of tools for system programming, mostly on
x86-64 architecture for now.

It provides direct access to Linux system calls (*syscalls*) on x86-64
architecture without going through the libc and without going through the
GHC FFI mechanism by default (so be careful not to use blocking system calls).

On top of that, it gives access to Linux subsystems. For now:

* input: events from keyboard, mouse, touchscreen, sound card (when a connector
  is plugged in/plugged out), etc.
* graphics: manage monitors and what is displayed on them

It provides support for some file formats used in system programming such as
ELF (with DWARF debug information), CPIO, etc. It provides a few modules to
deal with binary formats on top of Data.Binary that are used internally. It
supports some compression algorithms and containers (Deflate, GZip, etc.)
used in other file formats.

Finally, as ViperVM started as a runtime system for heterogeneous
architectures (GPGPU, etc.), it provides modules to dynamically load and use
OpenCL libraries. *Platform* modules are an attempt to abstract over
memories and processors of heterogeneous architectures to automatically
handle computations and data transfers. The ultimate goal is to provide an
execution model using parallel functional programming, but we are not there
yet. *Library* modules are supposed to handle a library of computational
kernel sources (OpenCL, C, etc.). Compiled kernels are stored to avoid
superfluous compilations.

## Examples

### ELF Web

ELFWeb program can be used to navigate into a ELF binary file. Use your Web
browser to see the result.

```bash
ELFWeb -p 8020 ./mybinary &
firefox http://localhost:8020
```

### Platform Web

PlatformWeb gives basic information about the platform (memories, processors,
networks). It can also be used to perform basic operations (e.g. memory
allocation/release) for test purpose.

```bash
PlatformWeb -p 8020
firefox http://localhost:8020
```

### GUnzip

Simple decompressor for the GZip format.

```bash
tar czf test.tgz # some files...
gunzip test.tgz
```


### udev

Dump kernel system events (i.e. changes into the system tree) on standard output.

```bash
udev
# try plugging or unplugging a device (USB key, etc.)
```
