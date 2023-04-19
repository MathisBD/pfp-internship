# Code structure :

The IR resembles an SSA IR : 
- Code is organized in lists of statements, each being a binding of an expression to one or several variables.
- Each variable should be bound only once in the entire program.
- Expressions are "flat", i.e. all subexpressions are 'SubExp's, i.e. variables or scalar constants.
- There are no tuples. Expressions can return several values. SOACS can take as input and return several arrays/values.

Primitive (scalar) values/types, basic IR : 
- Language.Futhark.Core
- Language.Futhark.Primitive

Core IR definition :
- Futhark.IR.Syntax.Core / Futhark.IR.Syntax + many other small files (for shapes / types etc.)
- Futhark.IR.Rep : RepTypes class
- Futhark.IR.SOACS : RepTypes instance used for the main optimization passes

Basic operations on the IR :
- Futhark.IR.Traversals
- Futhark.Transform.* : rename / substitute variables, basic simplifications
- Futhark.Builder : build IR fragments, automatically ensuring that all variable names are distinct

Simplification : implemented as a pass.
Simplification is used on many different reps : what changes is the rule book and some simplification parameters.
- Futhark.Optimise.Simplify.Rule : definition of what a simplification rule is
- Futhark.Optimise.Simplify.Rules/Futhark.Optimise.Simplify.Rules.* : a set of common simplification rules.
- Futhark.Optimise.Simplify.Rep : custom rep used by the simplification engine. This wraps the input rep with additional cached info.
- Futhark.Optimise.Simplify.Engine : actual simplification engine. Applies the given rules + performs CSE and loop hoisting and inlining.
- Futhark.Optimise.Simplify : simplification driver. The simplification engine is applied until convergence is reached.

# General compiler organization :

Front end (mostly in Futhark.Language.*)
- Parse
- Remove modules
- Monomorphize
- Defunctionalize 
- Internalize : produce core IR in SOACS rep

Middle/back end : apply optimization passes on the core IR
- a single pass = a mapping Prog fromrep -> Prog torep
- general purpose passes (simplification/inlining/fusion/etc) work on the SOACS rep
- lower level passes (e.g. kernel extraction, memory allocation) work on representations closer to their target machine model
- passes are sequentialized in "pipelines" : each backend (GPU/sequential/Multicore) is implemented as a different pipeline

