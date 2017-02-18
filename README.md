# Haskus system

Haskus system is a framework written in Haskell that can be used for system
programming. Fundamentally it is an experiment into providing an integrated
interface leveraging Haskell features (type-safety, STM, etc.) for the whole
system: input, display, sound, network, etc.

It is based directly and only on the Linux kernel:

* it doesn't contain any kernel level code (device driver, etc.)
* it doesn't rely on usual interfaces (e.g., libdrm, libinput, X11, wayland,
  etc.) to communicate with the kernel

Note that it still depends on GHC's RTS dependencies (libc, etc.).

Some modules are quite orthogonal to the aim of the project and can be used
independently. E.g.,

* the [Variant](src/lib/Haskus/Utils/Variant.hs) type described
  [here](http://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-2.html)
* the [file formats](src/lib/Haskus/Format) modules
* the [memory layout](src/lib/Haskus/Format/Binary) modules to easily match C
  data types (struct, unions, bit fields, etc.) from Haskell code
* an [x86-64 disassembler](src/lib/Haskus/Arch/X86_64/Disassembler.hs)

The low-level Linux interface is in
[Haskus.Arch.Linux](src/lib/Haskus/Arch/Linux) and the fluctuating higher-level
interface on top of it is in [Haskus.System](src/lib/Haskus/System).

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
# Programs

Several utility programs are bundled with the framework:

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
