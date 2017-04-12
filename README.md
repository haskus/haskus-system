# haskus-system examples

This repository contains some examples using [haskus-system](http://haskus.org/system/).

Please read the [documentation](http://haskus.org/system/manual) to understand
how to build and execute them.


We provide a script that automatically performs some of the steps described in
the documentation. For instance, to build and execute the "Demo" program with
`QEMU`, use:

```bash
./build.sh qemu2/Demo
```

It will download and build `Linux`, `Syslinux` and the required Haskell packages
(using `stack`) such as the haskus-system package. You may need to press "Enter"
several times when the Linux kernel is configured. Finally it runs the selected
system with `QEMU`.
