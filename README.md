# Haskus system

Haskus system is a framework written in Haskell and designed for system
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

* the [file formats](src/lib/Haskus/Format) modules
* an [x86-64 disassembler](src/lib/Haskus/Arch/X86_64/Disassembler.hs)

The low-level Linux interface is in
[Haskus.Arch.Linux](src/lib/Haskus/Arch/Linux) and the fluctuating higher-level
interface on top of it is in [Haskus.System](src/lib/Haskus/System).

Website: http://www.haskus.org/system

# How to build and install

Use stack commands to build Haskus system:

Build:
```bash
$ stack setup
$ stack build
```

Install:
```bash
$ stack install
```

Tests:
```bash
$ stack test
```

Benchmarks:
```bash
$ stack bench
```

# Building systems

Use ``haskus-system-build`` tool to build systems.

In a new directory do:
```bash
$ haskus-system-build --init
$ haskus-system-build --test
```

See the documentation on http://www.haskus.org/system

# Other programs

Several other utility programs are bundled with the framework:

#### haskus-elf

It can be used to navigate into a ELF binary file. Use your Web browser to see
the result.

```bash
$ haskus-elf -p 8020 ./mybinary &
$ firefox http://localhost:8020
```

#### haskus-system-info

Show info about the framework (e.g., supported x86 instructions).

```bash
$ haskus-system-info -p 8020 &
$ firefox http://localhost:8020
```

#### haskus-gunzip

Simple decompressor for the GZip format.

```bash
$ tar czf test.tgz # some files...
$ haskus-gunzip test.tgz
```

#### haskus-udev

Dump kernel system events (i.e. changes into the system tree) on standard output.

```bash
$ haskus-udev
$ # try plugging or unplugging a device (USB stick, etc.)
```
