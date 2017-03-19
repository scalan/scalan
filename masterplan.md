
Mini
====

Goals
-----
- provide extensible modular framework for meta-programming based on staged evaluation
- provide untyped core IR for Java based API
- provide typed IR wrappers for Scalan migration path
- provide generic composable transformers
    - isomorphic specialization
    - slicing
    - structure flattening
    - shaping
    - location specialization
    - inplacing using Updaters, Locations and Lenses
    - etc.
- support in-place operations
- generalize Scalan and provide easy migration path 
- support definition of composable finite state machines (FSM)
- support Iso-kernels
- make Elem and IR serializable
- save IR in persistent store with versioning
- define plugable backend API
- provide Java codegen as reference implementation
- compilation unit should be class (not just function)
- support generation of user-readable code
- fix long standing Scalan issues

Not Goals
---------
- sophisticated front-ends
- staging and deep embedding techniques 

Scalan issues that should be solved
------------------------------
+ remove Std traits 
+ make v.elem available in Abstract context (remove selfType1)
- avoid passing implicit Elems where possible
- simplify scalan-meta
- rename Elem -> Type
- avoid using Reflection API in IR
- separate typed staging from untyped core IR 
- more principled solution for getResultElem
- mechanism to map structs to given types (instead of generating Anon2390 classes)

Design decisions
----------------
- separate staging front-end from IR (typed wrappers over untyped core)
- core IR should be self-sufficient
- use LMS style staging in Scala for easy migration from Scalan
- Cake based modular design of staging front-end
- Java front-end using fluent API and builder pattern
- IR for Java subset of Scala
- nested classes and interfaces
- recursive functions
- first-class IR cakes so that all basis primitives are methods[<0;61;16M

What is removed from scalan-core
--------------------------------
- MethodMappingDSL
+ JNI
+ Array, List, ArrayBuffer, Map from basis
+ AbstractStrings
+ Std context
+ Effectful, EffectfulCompiler
- see also all [Mini] commits


