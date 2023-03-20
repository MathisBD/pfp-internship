--> A short introduction to the core IR

The basic building block is a statement ('Stm'), which consists in an expression and a binder. A binder in turn is simply a list of variables : an expression can return several values. Expressions ('Exp') are one of the following :
- A basic operation such as a unary/binary operator, a constant or a variable access.
- A match, with each branch being a body ('Body'), consisting in a list of statements.
- A loop, with a body.
- An accumulator : TODO
- A rep-specific operator ('Op'). For the 'SOACS' rep, this would be a SOAC with its input arrays and lambda function.

The IR is parameterized by a representation ('rep') : it defines what bookeeping information is kept in the IR, and the contents of the rep-specific 'Op' expression. Examples of reps would be 'SOACS', 'GPU', 'Seq'.

--> How the compiler manipulates the IR

The compiler pipeline is organized as a sequence of passes ('Pass'). Each pass takes as input a program in a rep and outputs a program in another rep ('Prop frep -> Prog trep'). Examples of passes would be simplification, inlining, fusion, memory allocation.

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


