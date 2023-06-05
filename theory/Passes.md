--> A short introduction to the core IR

The basic building block is a statement ('Stm'), which consists in an expression and a binder. A binder in turn is simply a list of variables : an expression can return several values. Expressions ('Exp') are one of the following :
- A basic operation such as a unary/binary operator, a constant, a variable access, basic array rearrange/copy etc.
- A match, with each branch being a body ('Body'), consisting in a list of statements.
- A loop, with a body.
- An accumulator : this is (for now) specific to Automatic Differentiation. Accumulators are essentially used to simulate accumulation effects, i.e. to allow functions to in-place update an array several times at the same index.
- A rep-specific operator ('Op'). For the 'SOACS' rep, this would be a SOAC with its input arrays and lambda function.

The IR is parameterized by a representation ('rep') : it defines what bookeeping information is kept in the IR, and the contents of the rep-specific 'Op' expression. Examples of reps would be 'SOACS', 'GPU', 'Seq', 'GPUMem'. Many reps are wrappers around some other rep : for instance 'GPUMem' simply adds a memory allocation operation to the 'GPU' rep.

--> A note on the SOACS rep

In this rep, potential parallelism is represented as SOACS : these are high-level descriptions of Map/Scan/Reduce/Scatter etc. A special SOAC is 'Stream' : semantically it is a fold of an arbitrary lambda that takes as input chunks of the input array, except that in the flattening phase it will be turned either into a sequential loop (chunk size = 1) apply the lambda to the whole array. The lambda usually contains another SOAC.

--> A note on the GPU rep

In this rep, parallelism is explicit : the most common construct is a 'SegOp', which roughly corresponds to a GPU kernel. A SegOp is a segmented map/scan/red/histogram, that executes at a given 'SegLevel'. The body of a SegOp can contain more parallelism in some specific case : for now the only one I have found is that a 'SegThreadInGroup' SegMap can be nested inside a 'SegGroup' SegMap (this is enforced in the type checking rules). I still don't know what the semantics of these are though.

Weirdly enough, there can still be plain high-level SOACS in this rep (using the 'OtherOp' constructor of 'HostOp'). I don't know yet what purpose they serve, but they are eventually sequentialised in the 'unstreamGPU' pass (before we convert to GPUMem rep).

--> A note on the GPUMem rep

This rep adds two concepts on the previous rep (GPU) :
- Memory allocations are directly represented in the IR using the 'Alloc' constructor of 'MemOp' (the GPUMem 'Op'). You specify the amount of bytes to allocate (this quantity can of course be dynamic, it uses a SubExp) and the memory space to allocate it in (global memory, shared memory, etc.). Allocating returns a new memory block.
- Arrays are associated to a memory block, and the mapping from the array index space to flat space is represented using an LMAD (linear memory access descriptor). An LMAD is essentially a kind of affine function 'Z^n -> Z'. This way we can represent row-major, column-major, or more exotic layouts simply by using different LMADs. Other easy constructs to represent are array rearranging (permuting the dimensions) and array slicing : we can statically compute the new LMAD. 

Read the recent paper 'Memory Optimizations in an Array Language' for more details on LMADs in Futhark.

An important detail is that arrays produced by expressions can be associated either to a specific memory block ('ReturnsInBlock') or to an existential memory block ('ReturnsNewBlock'), meaning that the binder can choose where to store the array and it will be constructed there directly.

--> How the compiler manipulates the IR

The compiler pipeline is organized as a sequence of passes ('Pass'). Each pass is a monadic program transformation, potentially changing the IR rep ('Prop frep -> PassM (Prog trep)'). Examples of passes would be simplification, inlining, fusion, kernel extraction, and various optimizations.

The different pipelines can be inspected in Futhark.Passes : every backend starts with the standard (simplify-inline-fusion) pipeline, followed by backend-specific passes.

--> A short description of every pass

The 'SimplifySOACS :: Pass SOACS SOACS' pass performs common simplification such as CSE, loop hoisting (directed by simple user rules), and applies user simplification rules (directed by a list of user rules).
Simplification rules are local, meaning they consume a statement and build another statement (or nothing at all), and have access to a symbol table (information about the types and binding circumstances of variables in scope) and a usage table (information about which variables in scope are used / consumed by subsequent statements).
An example of a simplification rule would be removing unnecessary array copying.

The 'inlineConservatively :: Pass SOACS SOACS' and 'inlineAggressively :: Pass SOACS SOACS' passes inline functions. If you see the call graph as a DAG, it starts by inlining the leaves and progresses upwards towards the roots.
The aggressive pass inlines everything that is not marked as #[noinline], while the conservative pass inlines everything that is marked #[inline] or has a very small body (in terms of number of statements) or is called only once. 
These passes also perform dead function removal, which can be used as its own pass 'removeDeadFunctions :: Pass SOACS SOACS'.

The 'performCSE :: Pass rep rep' pass performs common subexpression elimination. It first lifts the rep to 'Aliases rep' by performing alias analysis. Then CSE works by tracking what pattern binds each expression (in a map), and replacing an expression by a variable access when it has already been computed at least once. Special care must be taken with certificates and memory accesses. Finally the program is lowered again to the initial rep.
You would typically run copy propagation on the output of CSE, as it will leave many expressions of the form 'let a = Var b'.

The 'fuseSOACs :: Pass SOACS SOACS' pass performs vertical and horizontal SOAC fusion. It first builds the dependency graph of the program (using a nice library for inductive graphs, see the paper 'Inductive Graphs and Functional Graph Algorithms' by Martin Erwig). It then locates all possible fusion pairs, both horizontal and vertical, and attempts to fuse them one after the other. When it is finished it linearizes the graph back to a program.

The 'extractKernels :: Pass SOACS GPU' pass performs moderate flattening, using a combination of loop-map interchange, loop-branch interchange, map distribution and other heuristics to extract as much top level parallelism as possible. It then extracts the kernels. This pass is very complex.

The 'optimiseGenRed :: Pass GPU GPU' and 'tileLoops :: Pass GPU GPU' are both related to loop tiling (1D and 2D, maybe also 3D ?).

The 'babysitKernels :: Pass GPU GPU' pass performs coallescing optimizations for memory reads. For each kernel, it fetches arrays that are defined outside the kernel but accessed inside, and attempts to make a transposed copy of the array (before the kernel) so that the kernel can access the transposed array in a coallesced pattern. It will make one copy of the array for each access in the kernel (and optimize away duplicate copies).
--> How does this work with LMADs ? This pass runs before memory and LMADs are introduced... (see comment above ExpMap in Futhark.Pass.KernelBabysitting) At the memory level it would be sufficient to enforce that the last thread index has a stride of 1 in the LMAD (and give up on cases where there are more than one LMAD). It is unclear to me how you would check this before memory information is introduced : does the babysitKernels pass simply discard such 'difficult' cases ?

The 'unstreamGPU :: Pass GPU GPU' pass simply sequentializes any remaining high-level SOACS.

The 'explicitAllocations :: Pass GPU GPUMem' pass adds allocation statements for each array creation, and all index functions/LMAD information. 
An interesting point is how to handle match cases (and loops), that may return arrays that may have different index functions and live in different memory blocks. This is handled by augmenting each match case to return existential index functions (i.e. with 'holes') alongside with the information needed to fill in the holes in the index function (the context). This of course requires that each branch of the match returns the same existential index function (i.e. with the same holes). When binding the match expression, we also add variables to bind the context and the holes in the index function are filled in with these variables.

The 'liftAllocationsGPUMem :: Pass GPUMem GPUMem' very simple pass rearranges the order of statements within a block (i.e. not accross loop/match/function boundaries) so that memory block allocations come first. This is probably useful for several later optimizations : I can say for sure that it is used in the array short-circuiting pass.

The 'lowerAllocationsGPUMem :: Pass GPUMem GPUMem' is the opposite of the previous one : it pushes allocations as low as possible (towards their first use point), but not accross loop/match/function boundaries. 

The 'ArrayShortCircuiting.optimiseGPUMem' pass tries to remove unnecessary copies by constructing an array directly in the memory block it will be copied to. See the LMAD paper for more details.

Array combinators on the GPU

Optimal usage of the memory system is a key element of fast GPU algorithms. Unfortunately for many common algorithms this is hard to achieve and time consuming when done naively, even when using a high-level programming language such as Futhark. In this talk I will describe a set of high level array combinators (similar to map, reduce, filter, etc.) that raise the level of abstraction when thinking about memory access patterns on the GPU, but still allow for compilation to efficient GPU code. These combinators also give rise to a rich fusion algebra which will prove useful when compiling them.



