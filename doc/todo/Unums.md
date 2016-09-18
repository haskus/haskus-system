# TODO for Unums

#### Compute tables

We should find ways to automatically populate look-up tables for all operations.


### SORN

In Unums v2.0, SORN are implemented as bit sets: 1 bit per Unum. For a n bits
Unum, the SORN is 2^n large.

E.g., 8-bit  unum means 256-bit        SORN
      16-bit unum means 65536-bit      SORN (8 kB)
      24-bit unum means 16777216-bit   SORN (2 MB)
      32-bit unum means 4294967296-bit SORN (512 MB)

Pros:
   * manipulation is easy (bit operations)
   * manipulation time is fast and constant (no indirection, etc.)
   * precise set of Unum values
Cons:
   * size may be too big (especially in look-up tables: 2^(3n) bits for a
full table)

We could find other SORN implementations with different trade-offs.

#### Contiguous SORN 

We can encode contiguous SORN with two values:
   * start: the starting unum
   * count: the number of unums from start upwards

Pros:
   * size is much smaller (2 * unum size),  especially for look-up tables because
   connected sets remain connected under addition, subtraction, multiplication
   and division.
   * trivial logic for negate and reciprocate (i.e., operate on bounds only)
Cons:
   * logic is a little bit more complicated because we have to mix up connected and disjoint sets

#### SORN as bloom-filters

In many cases, we can use Unums and SORN to reduce a search space: the computed
solution gives us a SORN containing the solution and we may need to refine each
Unum in the SORN to find it precisely.

In these case, we could use a bloom-filter as an implementation for the SORN: it
would potentially contain false positives but not false negative.

Pros: much smaller SORN size (in bits)
Cons: potential false positives

