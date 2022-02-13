# rburg
rburg is a instruction selection generator using dynamic programming to cover Directed Acyclic Graphs in the same family as burg, iburg, lburg and various others. It is a subcomponent of the [utcc compiler](https://github.com/lotrbuilders/Compiler-Project) and is purpose built for said IR.

## Workings
rburg is a procedural macro that automatically generates an instruction labeller and multiple other components necessary for a backend. It takes in a DSL description of the backend, parses this using [syn](https://crates.io/crates/syn) and performs some semantic analysis on the resulting datastructures. Using [quote](https://crates.io/crates/quote) it then generates a labelling phase, the necessary subroutines for an instruction selection phase, an interface usable by a register allocator and a final phase to emit assembly. 

## Licensing
rburg is licensed under MPL-2.0
