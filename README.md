# Haskus System examples

This repository contains some examples using [Haskus System](http://haskus.org/system/).

For instance, to build and execute the "Demo" program with QEmu, use:

```bash
./build.sh qemu2/Demo
```

It will download and build `Linux`, `Syslinux` and the required Haskell packages
(using `stack`) such as the Haskus System package. You may new to press "Enter"
several times when the Linux kernel is configured. Finally it runs QEmu.


## Building a bootable disk (/dev/XXX)

You have to do once:

```bash
./build.sh disk/PROGRAM
sudo _sources/syslinux-*/bios/linux/syslinux -iam -d /boot/syslinux /dev/XXX 
```

And then to update

```bash
./build.sh disk/PROGRAM
sudo mount /dev/XXX /mnt/disk
cp -rf _build/disk/PROGRAM/* /mnt/disk
sudo umount /mnt/disk
```


## Building a bootable CD-ROM (iso)

```bash
./build.sh iso/PROGRAM
dd bs=4M if=_build/isos/PROGRAM.iso of=/dev/XXX status=progress && sync
```

