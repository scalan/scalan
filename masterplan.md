
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
+ avoid passing implicit Elems where possible
- in boilerplate elements of constructors (see mkBind) are passed as implicits
- more principled solution for getResultElem
- simplify scalan-meta
- rename Elem -> Type
- avoid using Reflection API in IR
- separate typed staging from untyped core IR 
- mechanism to map structs to given types (instead of generating Anon2390 classes)


Design decisions
----------------
- LMS style staging in Scala for easy implementation of basis 
- Cake based modular design of staging front-end
- new dynamic IR to represent OOP constructs
- dynamic IR should be serializable
- classes and interfaces should form Symbol hierarchy (as scala.reflect.api.Symbols)
- Java front-end using fluent API and builder pattern to directly construct IR
- IR should be enough for Java subset of Scala (not full Scala)
    - nested classes and interfaces
    - recursive functions
- statically predefined staged classes (created with scalan-meta) should be wrappable in new dynamic IR 
- Scalan cake should be mixed with Def so that all its methods are subjects of staged invoke

What is removed from scalan-core
--------------------------------
- MethodMappingDSL
+ JNI
+ Array, List, ArrayBuffer, Map from basis
+ AbstractStrings
+ Std context
+ Effectful, EffectfulCompiler
- see also all [Mini] commits


