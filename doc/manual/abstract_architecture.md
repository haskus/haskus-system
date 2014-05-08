# Abstract architecture

## The big picture

Recent multi-core architectures are composed of several memory nodes
interconnected through networks with complex topologies. We can basically
represent them with a graph whose vertices are memory nodes (memories for
short) and edges are links between them. 

![Graph of memories](images/abstract_architecture1.png)

Some links between memory nodes share the same physical network. For instance a
bus or a physical link between two network cards. Hence, we add the notion of
network to our abstract architecture in order to know the kind of network we
deal with and how data transfers interact with each other.

![Graph of memories with networks](images/abstract_architecture2.png)

To each memory node, we associate "processors" that are able to transform data
contained in memories they are attached to. Some memory nodes may not have any
processor attached (e.g. disks). What exactly a processor is depends on the
hardware.

![Graph of memories with processors](images/abstract_architecture3.png)


## Memories

TODO: peers, buffers, regions
