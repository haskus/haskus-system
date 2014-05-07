# ViperVM

ViperVM stands for Very High PERformance Virtual Machine. Our aim with this
project is to provide a fully integrated environment to exploit current
architectures, especially high-performance ones (e.g. so caleld heterogeneous
architectures with GPUs or other accelerators, NUMA architectures, etc.).

We start from the ground up (bare metal) and build abstraction layers on top of
it. It allows us to fully control what is happening at every level. By written
this software using a high-level language (Haskell) we aim to make each part
easily composable so that it can be tweaked and optimized easily.

Finally, the ultimate goal is to provide a high-level environment for end-users
(scientists from other fields that should not have to deal with low level
computer architecture concerns).

Note that this software is still in early development.

[Abstract architecture](abstract_architecture.md)
