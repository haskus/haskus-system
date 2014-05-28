# Memory (low-level)

A memory is basically a sequence of cells into which applications can read and
often write values. It can be any kind of storage (RAM, hard disk, etc.).

## Pages / Blocks

Memories are most often managed on a page (e.g. in RAM) or block/sector (e.g.
on hard disk) basis. Page sizes range from a few bytes, to a few gigabytes
depending on the architecture. Pages with different sizes can co-exist on some
architectures.

Attributes can be defined for each page such as access protection, owner
process, etc. In addition, virtual memory mechanism is often used by operating
systems to allow pages to be moved to different memories such as hard disks
(swap out) and brought back on-demand, hence providing a kind of "infinite
memory" feeling on architectures supporting this. ViperVM aims to bypass this
swap mechanism as it should know better than the OS how to manage data.

## Buffers

Allocating memory consists in reserving a set of pages and considering them as
consecutive when their cells are addressed. In ViperVM, a `Buffer` entity
represents a memory allocation. It is only defined by its size (the sum of the
sizes of the reserved pages) and the memory it is allocated in, other fields
are architecture specific.

A buffer can be released, meaning that its reserved pages can be reused for
another allocation.

## Regions

Regions are sets of cells in a buffer. A region is defined by the offset of its
first cell in the buffer and by a shape. ViperVM currently defines two shapes:
* Shape1D: a set of consecutive cells, hence only defined by the number of cells
* Shape2D: a rectanguler set of cells. That is, several consecutive sets of
  cells all containing the same amount of cells, separated by a fixed number of
  cells. Hence it is defined by the number of "rows", the size of the rows and
  the number of padding cells between two rows.

Other shapes could be defined but these two regular shapes are the most
important because some networks are able to transfer regions with these shapes
from one memory to another. Hence they can be used to identify source and
destination cells easily.
