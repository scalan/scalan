# Scalan Community Edition manual

## Intro

Scalan is a framework for creating staged embedded DSLs in Scala. It allows you to write high-level Scala programs and compile them to efficient parallel C++ (or other languages, depending on the backend) and to develop new abstractions which can be used in such programs.

### Installation

[SBT](http://www.scala-sbt.org/) is required to build Scalan. See linked documentation for installing and using SBT itself. There is also [an improved runner](https://github.com/paulp/sbt-extras) which takes care of downloading correct SBT version and adds some extra features.

The project consists of three subprojects: `core`, `community-edition`, and `lms-backend`, as well as the aggregate project `all`. `lms-backend` currently depends on a fork of [LMS](https://github.com/TiarkRompf/virtualization-lms-core/) located at <http://github.com/scalan/virtualization-lms-core>, branch `scalan-develop`.

The tests are split into unit tests (which can be run with `test` SBT command) and integration tests (`it:test`).

## Writing programs

Scalan is embedded into Scala, so all standard rules of Scala syntax apply and knowledge of Scala is assumed in this manual. However, it only supports a limited subset of types and operations. Note that if you write a program which is legal Scala, but not legal Scalan, there are currently no error messages or warnings. This is planned to be changed in the future.

### Types

All Scalan values have type `Rep[A]` for some Scala type `A`. Currently `A` can be one of the following types:

1. Base types: `Boolean`, `Byte`, `Int`, `Long`, `Float`, `Double`. `String` will be added soon.
2. Pairs `(A, B)`. Larger tuples are are represented as pairs nested to the right, so e.g. instead of `Rep[(Int, Int, Boolean)]` you write `Rep[(Int, (Int, Boolean))]`.
3. Sums `Either[A, B]` (can also be written as `A | B`).
4. Functions `A => B`. Functions with multiple arguments are emulated by functions taking a tuple (or by currying), such as `Rep[((A, B)) => C]` (note double parentheses).
5. Arrays `Array[A]` (in `community-edition` subproject).
6. Traits and classes added by DSL developers (see "Extending Scalan" section below).

Note that nested `Rep` is not allowed. There are type aliases for some `Rep` types, such as `type IntRep = Rep[Int]`, `type Arr[A] = Rep[Array[A]]`, etc.

Scala values of type `A` can be converted to `Rep[A]` implicitly (or explicitly using `toRep` method if desired). However, it's impossible to convert from `Rep[A]` to `A`.

### Operations

Scalan supports the usual arithmetic, logical, ordering, etc. operations. They are added by implicit conversions on `Rep`, so e.g. `y + x` where `x: Rep[Int]` and `y: Int` doesn't compile; write `toRep(y) + x` or `x + y` instead. There are currently no implicit widening conversions from `Rep[Int]` to `Rep[Long]`, from `Rep[Float]` to `Rep[Double]`, etc. Methods like `toInt` and `toDouble` should be used instead.
~~~scala
val x: Rep[Int] = 1

x + 3
~~~
As in Scala, arithmetical operations on `Rep[T]` require an implicit `Numeric[T]` (`Fractional[T]` for `/`) to be in scope, and ordering operations require an `Ordering[T]`.

Equality is written `===` and inequality `!==`. Note that accidental use of `==` or `!=` will likely compile (since `Boolean` can be implicitly converted to `Rep[Boolean]`) but produce wrong results!

Conditional expression is `IF (cond) THEN branch1 ELSE branch2`, where `cond: Rep[Boolean`. `THEN` is optional, and `ELSEIF` can be used in place of `ELSE`. Because `IF`, `THEN`, and `ELSE` are methods, they are parsed differently from `if` and `else`. Namely, `THEN` and `ELSE` shouldn't start new lines, e.g.
~~~scala
IF (true)
  THEN true
  ELSE false
~~~
is parsed by `scalac` as 3 separate expressions and doesn't typecheck. Instead write
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

For tuples the usual Scala methods `_1`, `_2`, etc. are available. Tuples can be constructed and pattern-matched using `Pair` and `Tuple` objects.
~~~scala
val a0 = 1
val b0 = 2
val c0 = false
val tuple: Rep[(Int, (Int, Boolean))] = Tuple(a0, b0, c0)

val Tuple(a1, b1, c1) = tuple
val c2 = tuple._3
~~~
Currently tuples up to size 8 are supported.

Scalan function values must be pure (support for limited side effects is planned). They can be obtained from pure Scala functions which take and return `Rep` by an implicit conversion `fun`. If a function shouldn't be inlined, `funGlob` method can be used instead.
~~~scala
val f: Rep[Int => Int] = { x: Rep[Int] => x + 1 } // or val f = fun { x: Rep[Int] => x + 1 } or val f = fun((_: Rep[Int]) + 1)
~~~

Loops can be written as `from(startingState).until(isMatch)(step)`, where `isMatch` and `step` are pure Scala functions which take the same number and types of arguments as passed to `from` and return `Rep[Boolean]` and a tuple as above respectively.
~~~scala
val collatz = from(start).until(_ === 1) { n => IF (n % 2 === 0) THEN (n / 2) ELSE (n * 3 + 1) }
~~~

Methods can be defined using `def` keyword, as usual. Normally all arguments and return values will have `Rep` types, but this isn't required.

#### User types

User types (abstract like `Vector[T]` and concrete like `DenseVector[T]`) are introduced as part of a DSL (`VectorsDsl`). For these types `T` there is an implicit conversion from `Rep[T]` to `T`, so all methods/fields of `T` are available on `Rep[T]`. If you need to add your own types see "Extending Scalan" below. It's possible to create classes with `Rep` fields, but you won't be able to stage these classes directly (i.e. obtain a `Rep[MyClass]`).

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

Now, there are two ways in which Scalan can work with this program:

### Sequential mode

Run it without optimizations in order to ensure it works as desired and debug if necessary. This is done by mixing in `ScalanCommunityDslSeq` (and `Seq` versions of any additional DSLs used by your program):
~~~scala
object HelloScalanSeq extends HelloScalan with ScalanCommunityDslSeq {
  def main(args: Array[String]) = run(...)
}
~~~
In this mode, Scalan's behavior is very simple: `Rep[A]` is the same type as `A`, and `fun` returns its argument, so you can mentally erase all `Rep` and `fun`. However, the structure of Scalan programs inhibits some of Scala's own optimization opportunities, so it should be expected to run somewhat slower than an equivalent Scala program.

### Staged mode

Compile it to produce optimized code by mixing in `ScalanCommunityDslExp` (and `Exp` versions of any additional DSLs) and a backend trait. Currently Scalan Community edition contains only one backend `LmsBackend`.
~~~scala
object HelloScalanStaged extends HelloScalan with ScalanCommunityDslExp with LmsBackend {
  def main(args: Array[String]) = // TODO
}
~~~
Running this program will generate Scala code in directory TODO, which still needs to be compiled or added to your own program (in the future the generated code will be automatically compiled and a .jar file created). Note that generated code does _not_ depend on Scalan or any other external libraries.

In this mode `Rep[A]` should be understood as representing a value of type `A` in the generated code. Any values of non-`Rep` Scala types which appear in the Scalan program aren't directly represented.

Scalan aggressively applies optimizations such as dead code elimination, common sub-expression elimination, and function inlining independently of backend. The backend can, of course, include its own optimizations as well (a major one in LMS is loop fusion).

## Understanding Scalan code

Scalan uses a variant of [cake pattern](http://www.cakesolutions.net/teamblogs/2011/12/19/cake-pattern-in-depth) for code organization. Namely, it is composed of a set of traits such as `Base`, `Elems`, etc. which define a component API (and helper methods using this API). Each component has two implementations `ComponentNameSeq` and `ComponentNameExp` which are used in sequential and staged mode respectively. There is a trait combining all components called `Scalan`, and corresponding `ScalanSeq` and `ScalanExp` traits. These traits are used as self-types for the components and their implementations, which allows each component to depend on all others. There are also `ScalanCtxSeq` and `ScalanCtxExp` which extend `ScalanSeq` and `ScalanExp` with implementations.

`community-edition` subproject adds more components. Their combination with `Scalan` is called `ScalanCommunity` (also with `Seq` and `Exp` versions). It also defines some DSLs which work the same as components, and `ScalanCommunityDsl` combines `ScalanCommunity` and all DSLs.

Important types to understand are: 

* `Element[A]` is a reified type representation. For all legal Scalan types `Rep[A]` an `Element[A]` instance exists and should be available implicitly.
* `Exp[A]` is `Rep[A]` in staged mode. As said above, it represents a staged value (i.e. a value in generated code) of type `A` or, more strictly speaking, an identifier of such a value.
* `Def[A]` is a definition of an `Exp[A]`.

## Extending Scalan

New methods can be added to existing types using implicit conversions:
~~~scala
implicit class Norm(arr: Rep[Array[Double]]) {
  def norm: Rep[Double] = Math.sqrt((arr *^ arr).sum)
}

val x: Rep[Array[Double]] = ...
x.norm
~~~
Normal Scala rules apply.

It's alse possible to add new user types to Scalan. As a simple example, we consider 2d points. We have a trait describing the interface, and a class describing an implementation (though in this case there is only one implementation, the split is still required). They are contained in the trait `Points` which serves as a DSL component.
~~~scala
trait Points { self: PointsDsl =>
  trait Point {
    def x: Rep[Double]
    def y: Rep[Double]
    def distance(other: Rep[Point]): Rep[Double] = Math.sqrt((x - other.x)*(x - other.x) + (y - other.y)*(y - other.y))
  }
  trait PointCompanion

  abstract class PointImpl(val x: Rep[Double], val y: Rep[Double]) extends Point
  trait PointImplCompanion
}

trait PointsDsl extends Scalan with impl.PointsAbs
trait PointsDslSeq extends PointsDsl with impl.PointsSeq
trait PointsDslExp extends PointsDsl with impl.PointsExp
~~~
This obviously doesn't compile yet, because of references to non-existent classes in the `impl` package. They are boilerplate code which must be generated using the `scalan-meta` tool available at <http://github.com/scalan/scalan-meta> (see its documentation). It will have to be regenerated if `Point` or `PointImpl` are renamed or deleted, or if new implementations of `Point` are added.

Note that methods in `Point` must have `Rep` in argument types and return value type. If `Point` had a type parameter, it would also have methods asserting existence of `Element` instances. It may seem some of these empty traits are unnecessary, but they serve as extension points. E.g. any methods added to `PointCompanion` will be available on `Point` companion objects.

See `scalan.linalgebra.Vectors` for a larger example.

Adding new primitive operations, core types, or backends to Scalan is possible, but not supported at the moment.

## See also

* [LMS](https://github.com/TiarkRompf/virtualization-lms-core/) is a source of many ideas in Scalan and is used as one of the backends.