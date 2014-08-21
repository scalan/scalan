# Scalan manual

## Intro

Scalan is a framework for creating staged embedded DSLs in Scala. It allows you to write high-level Scala programs and compile them to efficient parallel C++ (or other languages, depending on the backend) and to develop new abstractions which can be used in such programs.

### Installation

[SBT](http://www.scala-sbt.org/) is required to build Scalan. See linked documentation for installing and using SBT itself. There is also [an improved runner](https://github.com/paulp/sbt-extras) which takes care of downloading correct SBT version and adds some extra features.

The project consists of three subprojects: `core`, `community-edition`, and `lms-backend`, as well as the aggregate project `all`. `lms-backend` currently depends on a fork of [LMS](https://github.com/TiarkRompf/virtualization-lms-core/) located at <http://10.122.85.33:81/scalan-lite/lms>, branch `scalan-develop`.

The tests are split into unit tests (which can be run with `test` SBT command) and integration tests (`it:test`).

## Writing programs

Scalan is embedded into Scala, so all standard rules of Scala syntax apply and knowledge of Scala is assumed in this manual. However, it only supports a limited subset of types and operations. Note that if you write a program which is legal Scala, but not legal Scalan, there are currently no error messages or warnings. This is planned to be changed in the future.

### Types

All Scalan values have type `Rep[T]` for some Scala type `T`. Currently the following Scala types are supported:

1. Base types: `Boolean`, `Byte`, `Int`, `Long`, `Float`, `Double`. `String` will be added soon.
2. Pairs `(A, B)`. Larger tuples are emulated by nesting as described below.
3. Sums `Either[A, B]` (can also be written as `A | B`).
4. Functions `A => B`. Functions with multiple arguments are emulated by functions taking a tuple (or by currying), such as `Rep[((A, B)) => C]` (note double parentheses).
5. Arrays `Array[A]` (in `community-edition` subproject).
6. Traits and classes added by DSL developers (see "Extending Scalan" section below).

Note that nested `Rep` is not allowed. There are type aliases for some `Rep` types, such as `type IntRep = Rep[Int]`, `type Arr[A] = Rep[Array[A]]`, etc.

Scala values of type `T` can be converted to `Rep[T]` implicitly (or explicitly using `Base.toRep` method if desired).

### Operations

Scalan supports the usual arithmetic, logical, etc. operations. 

Equality is written `===` and inequality `!==`. Note that accidental use of `==` or `!=` will likely compile (since `Boolean` is converted to `Rep[Boolean]`) but produce wrong results! 

Conditional expression is `IF (cond) THEN branch1 ELSE branch2`, where `cond: Rep[Boolean`. `THEN` is optional, and `ELSEIF` can be used in place of `ELSE`. Because `IF`, `THEN`, and `ELSE` are methods, they are parsed differently from `if` and `else`. `THEN` and `ELSE` shouldn't start new lines, e.g.
~~~scala
IF (true)
  THEN true
  ELSE false
~~~
is considered as 3 separate expressions and doesn't typecheck. Instead write
~~~scala
IF (true) THEN true ELSE false
~~~
or
~~~scala
IF (true)
  THEN {
    true
  } ELSE {
    false
  }
~~~

Scalan function values must be pure (support for limited side effects is planned) can be obtained from Scala functions which take and return `Rep` by an implicit conversion `fun`. If a function shouldn't be inlined, `funGlob` method can be used.

Tuples are represented as pairs nested to the right, so e.g. instead of `Rep[(Int, Int, Boolean)]` you write `Rep[(Int, (Int, Boolean))]`. The usual Scala methods `_1`, `_2`, etc. are available. Tuples can be constructed and pattern-matched using `Pair` and `Tuple` objects (TODO add examples). Currently tuples up to size 8 are supported.

Loops can be written as `from(startingState).until(isMatch)(step)`, where `isMatch` and `step` are pure Scala functions which take the same number and types of arguments as passed to `from` and return `Rep[Boolean]` and a tuple as above respectively.

### Program structure and example

Your program needs to extends `Scalan` trait (along with any traits describing the DSLs you use) or `ScalanCommunity` if you want to use `community-edition` instead of `core`. Here is a very simple example program:
~~~scala
trait HelloScalan extends ScalanCommunityDsl { // ScalanCommunityDsl includes ScalanCommunity and all DSLs defined in that project
  def multMatrixVector[A](m: Rep[Matrix[A]], v: Rep[Vector[A]]): Rep[Vector[A]] =
    DenseVector(m.rows.map(row => (row ^* vector).sum))

  lazy val run = fun { x: Rep[(PArray[PArray[Double]], PArray[Double])] => } // TODO
}
~~~
This can be seen to be very close to a usual Scala program, except for use of `Rep` type constructor and `fun` method.

### Executing programs

Scalan can work with this program in one of the two modes:

1. __Sequential mode__. Run it in order to ensure it works as desired and debug if necessary. This is done by mixing in `ScalanSeqImplementation` (or `ScalanCommunityDslSeq`):
~~~scala
object HelloScalanSeq extends HelloScalan with ScalanSeqImplementation {
  def main(args: Array[String]) = run(...)
}
~~~
In this mode, Scalan's behavior is very simple: `Rep[A]` is the same type as `A`, and `fun` returns its argument, so you can mentally erase all `Rep` and `fun`. However, the program is not optimized and structure of Scalan inhibits some of Scala's own optimization opportunities, so it should be expected to run somewhat slower than an equivalent Scala program.

2. __Staged mode__. Compile it to produce optimized code by mixing in `ScalanStagedImplementation` or `ScalanCommunityDslStaged` and a backend trait:
~~~scala
object HelloScalanStaged extends HelloScalan with ScalanCommunityDslStaged with LmsBackend {
  def main(args: Array[String]) = // TODO
}
~~~
Note that running this program generates code and compiles it. The compiled code will be available in directory TODO, as specified by the program. It can be run as follows TODO.

## Scalan internals

## Extending Scalan

## See also

* [LMS](https://github.com/TiarkRompf/virtualization-lms-core/) is a source of many ideas in Scalan and is used as one of the backends.