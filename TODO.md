# ViperVM TODO list #

## OpenCL ##

### Bypass the ICD (Installable Client Driver) ###

We should use the native implementations directly as we already use dynamic
linking and because the ICD is crap (bad support for different OpenCL versions,
etc.). We should support more than a single OpenCL library in the platform
configuration.

We could simulate the ICD behavior if necessary to automatically find clients
(using ICD files). In addition we could support the OCL_ICD_VENDORS
environnement variable (path to ICD files). 

Not very difficult to do. 

References:

* [http://www.khronos.org/registry/cl/extensions/khr/cl_khr_icd.txt]
* [https://forge.imag.fr/projects/ocl-icd/]

## CPU support ##

Support for various instruction sets: x86, Power, ARM, etc.

### Platform detection ###

* detect the topology: memories and PUs
* bench memory/caches accesses (latency, bandwidth, etc.)
* available units and instructions per PU: FP, SIMD, CRC, etc.

### Core control ###

* control core modes (boost, sleep, etc.)

### Memory management ###

* control memory allocation precisely: buffer alignement, etc.
* Buffers are only accessed by their handle: there is no inter-buffer pointer.
Hence we can move buffers when they are not in use.

* We can develop our own fast allocator

Ideally, we could bypass virtual memory
* DMA transfers require pinned memory (for performance)
* we perform swap in/swap out in other memories (no necessarily disks) ourselves
* we want to explicitly manage NUMA: allocations and transfers


### Kernels ###

* Kernels can only perform computations, i.e. no syscall (malloc, IO, etc.).
* They are stackless: memory usage is kwown (upper bound) before their execution.
* They cannot perform far calls/jumps.

#### Compilation ####

* position independant codes
* support for hand-written kernels (assembly language)
* higher level representations (subset of C, data parallel, etc.)
    * register allocation algorithms (shared with other backends)
    * use best available instructions (SIMD, etc.)
    * cache size/latency/bandwidth aware

#### Loading ####

* Use our own loader.
    * reuse already loaded kernels
    * NUMA support for kernel storage (store kernels in memory close to the PUs)

#### Execution ####

We can have a thread pinned on each PU with high priority that executes kernels
submitted to the PU.

The convention to find kernel parameters can be arbitrarily defined for each
kind of platform: registers (Pascal, x86-64), "stack" (C x86), structure
address passed in a register, etc.

## Kernel fusion ## 

Suppose we have a chain of kernels: A -> B -> C -> D
whose intermediate data are not used by any other kernel.
Sometimes this can be replaced by a more efficient kernel that performs the whole operation.
It avoids uncessary runtime system overhead between kernel executions (scheduling decisions, etc.)

### Functional model ###

* statically we can use rewrite rules to do it


## Out-of-core ##

Make ViperVM manage out-of-core memories such as disks.

* Direct disk access (/dev/sd*)
* Memory allocator
* Manage transfers between memory and disks

### Check-pointing ###

Support automatic check-pointing by storing state (remaining computation and
required buffers) in disk.
