# Haskus system

Haskus system is a framework written in Haskell and designed for system
programming. Fundamentally it is an experiment into providing an integrated
interface leveraging Haskell features (type-safety, STM, etc.) for the whole
system: input, display, sound, network, etc.

Website: http://www.haskus.org/system

Documentation: http://doc.haskus.org/manual/ 

# Building systems

The ``haskus-system-build`` tool (in the package of the same name) is the
preferred way to build systems.

Install it with ``stack install haskus-system-build``:

Then in a **new directory** do:
```bash
$ haskus-system-build init       # download system template
$ haskus-system-build test       # download, build and test system in QEMU
```

Stack installs programs into `~/.local/bin`, so you have to ensure that it is in
your path.

You may have to install missing programs (cpio, lzip, qemu, make, gcc, binutils,
gzip, etc.) for these commands to succeed.

# Hacking on haskus-system

Use stack commands to build the ``haskus-system`` package:

Build:
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

# Other programs

Several other utility or test programs are bundled with the framework:

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
