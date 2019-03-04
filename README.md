# Haskus system

Haskus system is a framework written in Haskell and designed for system
programming. Fundamentally it is an experiment into providing an integrated
interface leveraging Haskell features (type-safety, STM, etc.) for the whole
system: input, display, sound, network, etc.

Website: http://www.haskus.org/system

Documentation: https://docs.haskus.org/system.html

# Building systems

The [haskus-system-build](https://github.com/haskus/haskus-system-build.git)
tool (in the package of the same name) is the preferred way to build systems.

Install it with:

```bash
$ git clone https://github.com/haskus/haskus-system.git
$ cd haskus-system
$ stack install haskus-system-build
```

It will install the program into ~/.local/bin. Be sure to add this path to your
$PATH environment variable.

Then in a **new directory** do:
```bash
$ haskus-system-build init       # download default system template
$ haskus-system-build test       # download, build and test system in QEMU
```

You may have to install missing programs (cpio, lzip, qemu, make, gcc, binutils,
gzip, etc.) for these commands to succeed. See the
[documentation](http://doc.haskus.org/manual/system/volume1/automatic_building.html).


# Hacking on haskus-system

Use ``stack`` commands to build the ``haskus-system`` package:

```bash
$ stack build # build
$ stack test  # run tests
$ stack bench # run benchmarks
```
