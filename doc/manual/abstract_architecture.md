# Abstract architecture

Recent multi-core architectures are composed of several memory nodes
interconnected through networks with complex topologies. We can basically
represent them with a graph whose vertices are memory nodes (memories for
short) and edges are links between them. To each memory node, we associate
"processors" that are able to transform data contained in memories they are
attached to.

TODO: add picture

Some links between memory nodes share the same physical network. For instance a
bus or a physical link between two network cards. Hence, we add the notion of
network to our abstract architecture in order to know the kind of network we
deal with and how data transfers interact with each other.
