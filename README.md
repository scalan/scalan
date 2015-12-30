# Scalan Compilation Framework

[![Join the chat at https://gitter.im/scalan/scalan](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scalan/scalan?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/scalan/scalan.svg?branch=master)](https://travis-ci.org/scalan/scalan)
[![Stories in Ready](https://badge.waffle.io/scalan/scalan.png?label=ready&title=Ready)](https://waffle.io/scalan/scalan)

## Intro

Scalan is a framework for domain-specific compilation in Scala. It allows you to write high-level Scala programs and compile them to efficient low-level code (in any language supported by the existing backends) by applying domain-specific compilation techniques.

Scalan is based on [Polymorphic Embedding](http://dl.acm.org/citation.cfm?id=1449935) and [LMS-like](http://scala-lms.github.io/) staging. However, is contrast to LMS, Scalan doesn't rely on `Scala-virtualized` and works with `Scala 2.10+` compiler.

In conjunction with [Scalanizer](https://github.com/scalan/scalanizer) Scalan can be used to develop domain-specific JIT compilers for hot-spot optimization in Scala. (To get started checkout Scala Days [video](https://www.parleys.com/tutorial/program-functionally-execute-imperatively-peeling-abstraction-overhead-from-functional-programs) and [demonstration project](https://github.com/scalan/scalanizer-demo))

One of the distinguishing feature of Scalan is [Isomorphic Specialization](http://dl.acm.org/citation.cfm?id=2633632) a new specialization algorithm and technique which allows to perform cross-domain translations of programs. Thus it is possible to construct compilation pipelines with gradual lowering of domain-specific abstractions.

Please visit [Scalan Google Group](https://groups.google.com/forum/#!forum/scalan) for Scalan-related discussions. See also [Contributions](#contributions) below and get involved.

### Building the project and running tests

[SBT](http://www.scala-sbt.org/) is required to build Scalan. See SBT documentation for installation and usage instructions. There is also [an improved runner](https://github.com/paulp/sbt-extras) which takes care of downloading correct SBT version and adds some extra features.

The project consists of several [subprojects](http://www.scala-sbt.org/release/tutorial/Multi-Project.html), including the aggregate project `scalan`.

One of the subprojects, `scalan-lms-backend` currently depends on a fork of [LMS](https://github.com/TiarkRompf/virtualization-lms-core/) located at <http://github.com/scalan/virtualization-lms-core>, branch `scalan-develop`. If you want to use it, you need to clone and build this dependency first, since it isn't published in a public repository.

The tests are split into unit tests (which can be run with the usual `test` SBT command) and integration tests (`it:test`) which actually generate a program (using some backend) and test the generated code. As of this writing, the only backends(codegens) available are Scala- and C++-based LMS backends, both defined in the `scalan-lms-backend` subproject.

If you want to create your own project depending on Scalan, you should use `publishLocal` SBT command to publish Scalan artifacts to your local Ivy repository and add dependencies as usual:

~~~scala
def liteDependency(name: String) = "com.huawei.scalan" %% name % "0.2.9-SNAPSHOT"

lazy val core = liteDependency("scalan-core")
lazy val library = liteDependency("scalan-library")
lazy val meta = liteDependency("scalan-meta")

lazy val myProject = Project("myProjectName").settings(
  // or core, core % "test" classifier "tests" if you only need scalan-core
  libraryDependencies ++= Seq(library, library % "test" classifier "tests")
)

lazy val myMeta = Project("myMetaProjectName").settings(libraryDependencies += meta)
~~~

`"test"` dependencies allow reuse of Scalan's existing test infrastructure and aren't necessary if you don't need it. See [Extending Scalan](#extending-scalan) below for an explanation of `myMeta`.

If you also need to depend on `scalan-lms-backend`, you have to add [Scala-Virtualized](https://github.com/tiarkrompf/scala-virtualized) to the `settings` block above. See [Maven Repository](http://mvnrepository.com/artifact/org.scala-lang.virtualized) for the latest Scala-Virtualized version; as of this writing, 2.11.2 is the only version which can be used:

~~~scala
settings(...,
  libraryDependencies += liteDependency("scalan-lms-backend"),
  scalaVersion := "2.11.2",
  scalaOrganization := "org.scala-lang.virtualized")
~~~

<!-- TODO how best to make sure virtualized scala-{library/compiler/reflect}.jar are first in the classpath -->

Alternately, you can use [project references](http://www.scala-sbt.org/0.12.4/docs/Dormant/Full-Configuration.html#project-references) to Scalan in your build instead of `libraryDependencies` if you want changes to Scalan to be immediately visible to your project without a `publishLocal` step.

### Stability

Currently we are quite far from 1.0 and breaking changes can happen. In such a case we usually publish a new release.

## Writing programs
With the introduction of [Scalanizer](https://github.com/scalan/scalanizer) as a new frontend, the role of Scalan is shifted one level down in the middle part of the compilation pipeline, but it still can be used for development of new EDSLs without Scalanizer.

Scalan is a library in Scala, so all standard rules of Scala syntax apply and knowledge of Scala is assumed in this manual. However, Scalan supports only a limited subset of Scala types and operations. This is not a conceptual limitation and will improve overtime especially with the help of Scalanizer plugin.

Note that if you write a program which is legal Scala, but not legal Scalan, there are currently no error messages or warnings. This is one of the reasons and rationale for creating Scalanizer where such errors can easily be handled.

### Types

All Scalan values have type `Rep[A]` for some Scala type `A`. Currently `A` can be one of the following types:

1. Base types: `Boolean`, `Byte`, `Int`, `Long`, `Float`, `Double`, `String`.
2. Pairs `(A, B)`. Larger tuples are are represented as pairs nested to the right, so e.g. instead of `Rep[(Int, Int, Boolean)]` you write `Rep[(Int, (Int, Boolean))]`. This might look a bit annoying for a DSL front-end, but it actually pays-off at middle-end where generic transformations are implemented.
3. Sums `Either[A, B]` (can also be written as `A | B`).
4. Functions `A => B`. Functions with multiple arguments are emulated by functions taking a tuple (or by currying), such as `Rep[((A, B)) => C]` (note double parentheses).
5. Arrays `Array[A]` (in `community-edition` subproject).
6. Traits and classes added by DSL developers (see [Extending Scalan](#extending-scalan) section below).

Note that nested `Rep` is not allowed (i.e. you cannot write something like this `Rep[(Rep[Int], Double)]`), but it is possible to nest `Rep` in user defined classes (see [Extending Scalan](#extending-scalan)).

There are type aliases for some `Rep` types, such as `type IntRep = Rep[Int]`, `type Arr[A] = Rep[Array[A]]`, etc. You can use aliases freely and the only rule for type aliases is that they don't introduce nested `Rep`s described above.

Scala values of type `A` can be converted to `Rep[A]` implicitly (or explicitly using `toRep` method if desired). Like in LMS they become constants of the next stage. (`Const[A]` nodes of intermediate representation, IR). However, in current version it's not possible to convert from `Rep[A]` to `A` as it would mean running the corresponding IR.

### Operations

Scalan supports the usual arithmetic, logical, ordering, etc. operations. They are added by implicit conversions on `Rep`. Note that `x + y` where `x: Int` and `y: Rep[Int]` doesn't compile; write `toRep(x) + y` or `y + x` instead. There are currently no implicit widening conversions from `Rep[Int]` to `Rep[Long]`, from `Rep[Float]` to `Rep[Double]`, etc. Methods like `toInt` and `toDouble` should be used instead.
~~~scala
val x: Rep[Int] = 1

x + 3
~~~
As in Scala, arithmetical operations on `Rep[T]` require an implicit `Numeric[T]` (`Fractional[T]` for `/`) to be in scope, and ordering operations require an `Ordering[T]`.

Equality is written `===` and inequality `!==`. Note that accidental use of `==` or `!=` will likely compile (since `Boolean` can be implicitly converted to `Rep[Boolean]`) but produce wrong results! Unfortunately this is a deficiency of polymorphic embedding and can only be cured by Scalanizer with automatic virtualization.

Conditional expression is `IF (cond) THEN branch1 ELSE branch2`, where `cond: Rep[Boolean]`. `THEN` is optional, and `ELSEIF` can be used in place of `ELSE`. Because `IF`, `THEN` and `ELSE` are methods, they are parsed differently from normal Scala conditionals `if`, `then` and `else`. Namely, `THEN` and `ELSE` shouldn't start new lines, e.g.
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

Scalan function values can be pure or they can have effectful operations. Effectful operation are reified in the IR using LMS style `Reflect/Reify` nodes.

Scalan function values are of type `Rep[A=>B]` and are not the same as Scala functions (or lambdas) of type `Rep[A] => Rep[B]`. They can be obtained from pure Scala functions which take and return `Rep` by an implicit conversion `fun`.

By default if you apply Scalan function like `val y = f(x)` the function is inlined in the point of application. If the function shouldn't be inlined, `funGlob` method can be used instead.

~~~scala
val f: Rep[Int => Int] = { x: Rep[Int] => x + 1 } // implicit conversion
val f1 = fun { x: Rep[Int] => x + 1 } // explicit conversion
val f2 = fun((_: Rep[Int]) + 1) // using underscore
val f3 = funGlob({ x: Rep[Int] => x + 1 })
~~~

Loops can be written as `from(startingState).until(isMatch)(step)`, where `isMatch` and `step` are pure Scala functions which take the same number and types of arguments as passed to `from` and return `Rep[Boolean]` and a tuple as above respectively.
~~~scala
val collatz = from(start).until(_ === 1) { n => IF (n % 2 === 0) THEN (n / 2) ELSE (n * 3 + 1) }
~~~

Methods can be defined using `def` keyword, as usual. Normally all arguments and return values will have `Rep` types, but this isn't required.

### User types

User-Defined Types, UDTs, (abstractions like `Vector[T]` and concrete implementations like `DenseVector[T]`) are introduced as part of a module - logical group of Scala traits usually in a single file. Such modules in Scalan are also called DSLs to emphasise their domain specificity (`VectorsDsl`, `MatricesDsl`, etc). For such user defined types (like `Vector[T]`) there is an implicit conversion from `Rep[Vector[T]]` to `Vector[T]`, so all methods/fields of `Vector[T]` are available on `Rep[Vector[T]]`. This is of cause true for other user defined types not only for `Vector`. If you need to add your own types see [Extending Scalan](#extending-scalan) below.

There are certain rules how to declare UDTs that should be obeyed in order to make UDTs first-class citizens in Scalan framework. It's also possible to create regular Scala classes with `Rep` fields, but you won't be able to stage these classes directly (i.e. obtain a `Rep[MyClass]`) because necessary boilerplate will not be generated by `scalan-meta` for such declarations.

### Program structure and example

Your program needs to extends `ScalanDsl` trait (along with any traits describing the DSLs you use). Here is a very simple example program:
~~~scala
trait HelloScalan extends MatricesDsl {
  lazy val run = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matrix[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r))}), width)
    val vector: Vector[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }
  // example input
  val matrix = Array(Array(1.0, 2.0), Array(3.0, 5.0))
  val vector = Array(2.0, 3.0)
  val input = (matrix, vector)
}
~~~

It can be seen to be very close to a usual Scala program, except for use of `Rep` type constructor and `fun` method. Note that `run` takes core types as argument and returns core types, not matrices and vectors themselves.

This example is available [in the repository](lms-backend/tests/src/it/scala/HelloScalan.scala). Please raise an issue if you find it isn't up-to-date!

Now, there are two ways in which Scalan can work with this program:

#### Sequential mode

Run it without optimizations in order to ensure it works as desired and debug if necessary. This is done by mixing in `ScalanCommunityDslSeq` (and `Seq` versions of any additional DSLs used by your program):
~~~scala
// to run: scalan-lms-backend/it:runMain HelloScalanSeq
object HelloScalanSeq extends HelloScalan with MatricesDslSeq {
  def result = run(input)

  def main(args: Array[String]) = {
    println(result.mkString(","))
  }
}
~~~
In this mode, Scalan's behavior is very simple: `Rep[A]` is the same type as `A`, and `fun` returns its argument, so you can mentally erase all `Rep` and `fun`. However, the structure of Scalan programs inhibits some of Scala's own optimization opportunities, so it should be expected to run somewhat slower than an equivalent Scala program.

#### Staged mode

Compile it to produce optimized code by mixing in `ScalanCommunityDslExp` (and `Exp` versions of any additional DSLs) and a compiler trait.
~~~scala
// to run: scalan-lms-backend/it:runMain HelloScalanExp
object HelloScalanExp {
  // allows use of standard Scala library, commented out to make tests faster
  // override val defaultCompilerConfig = CompilerConfig(Some("2.11.7"), Seq.empty)

  val program = new HelloScalan with MatricesDslExp

  val compiler = new CommunityLmsCompilerScala(program)
  import compiler._
  import compiler.scalan._

  def result = {
    // output directory
    val dir = new File("it-out")
    val compiled = compiler.buildExecutable(
      dir,
      // generated class name
      "HelloScalan1",
      // function to compile
      run,
      // write .dot files containing graph IR with default settings
      GraphVizConfig.default)
    // not necessary if you just want to generate
    // and compile the program
    execute(compiled, input)
  }

  def main(args: Array[String]): Unit = {
    println(result.mkString(","))
  }
}
~~~
Running this program will generate `HelloScalan.scala` and `HelloScalan.jar` in the given directory. `HelloScalan` class will have a `apply((Array[Array[Double]], Array[Double])): Array[Double]` method which corresponds to the `run` function. You can add either the source code or the jar to your own programs and call the `apply` method from them.

Note that generated code depends only on the Scala standard library, not on Scalan. If it's acceptable for your program to depend on Scalan, it can also call `Backend.execute` method which loads the generated class and invokes the `apply` method.

In this mode `Rep[A]` represents a value of type `A` in the generated code. Any values of non-`Rep` Scala types which appear in the Scalan program aren't represented directly.

Scalan aggressively applies optimizations such as dead code elimination, common sub-expression elimination, and function inlining independently of backend. The backend can, of course, include its own optimizations as well (a major one in the LMS backend is loop fusion).

## Understanding Scalan code

Scalan uses a variant of [cake pattern](http://www.cakesolutions.net/teamblogs/2011/12/19/cake-pattern-in-depth) for code organization. Namely, it is composed of a set of traits such as `Base`, `Elems`, etc. which define a component API (and helper methods using this API). Each component has two implementations `ComponentNameSeq` and `ComponentNameExp` which are used in sequential and staged mode respectively. There is a trait combining all components called `Scalan`, and corresponding `ScalanSeq` and `ScalanExp` traits. These traits are used as self-types for the components and their implementations, which allows each component to depend on all others. There are also `ScalanCtxSeq` and `ScalanCtxExp` which extend `ScalanSeq` and `ScalanExp` with implementations.

`scalan-library` subproject adds more components. Their combination with `Scalan` is called `ScalanCommunity` (also with `Seq` and `Exp` versions). It also defines some DSLs which work the same as components, and `ScalanCommunityDsl` combines `ScalanCommunity` and all DSLs.

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

It's also possible to add new user types to Scalan. As a simple example, we consider points on a plane. We have a trait describing the interface, and a class describing an implementation (though in this case there is only one implementation, the split is still required). They are contained in the trait `Points` which serves as a DSL component.
~~~scala
trait Points { self: PointsDsl =>
  trait Point {
    def x: Rep[Double]
    def y: Rep[Double]
    def distance(other: Rep[Point]): Rep[Double] = Math.sqrt((x - other.x)*(x - other.x) + (y - other.y)*(y - other.y))
  }
  // trait PointCompanion // uncomment to add methods for companion object

  abstract class PointImpl(val x: Rep[Double], val y: Rep[Double]) extends Point
  // trait PointImplCompanion
}

// generated automatically if absent;
// you can add methods which shouldn't be staged or which have different
// implementations in Seq and Exp contexts here

// trait PointsDsl extends impl.PointsAbs
// trait PointsDslSeq extends impl.PointsSeq
// trait PointsDslExp extends impl.PointsExp
~~~
This obviously doesn't compile yet, because of references to non-existent classes in the `impl` package. They are boilerplate code which must be generated using the `meta` subproject. Currently this unfortunately has to be done manually by running SBT command `meta/run <configurations>`, where `<configurations>` is one or more configuration defined in [BoilerplateTool.scala](src/main/scala/scalan/meta/BoilerplateTool.scala). This has to be done when a DSL file is added or changed, or after changes in the `meta` subproject itself. Projects which depend on Scalan and add their own DSLs will normally also have a `meta` subproject with a dependency on `scalan-meta`.

Note that methods in `Point` must have `Rep` in argument types and return value type. If `Point` had a type parameter, it would also have methods asserting existence of `Element` instances. It may seem some of these empty traits are unnecessary, but they serve as extension points. E.g. any methods added to `PointCompanion` will be available on `Point` companion objects.

See `scalan.linalgebra.Vectors` for a larger example.

Adding new primitive operations, core types, or backends to Scalan is possible, but the API is currently not stable.

## Contributions

Please feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Minor pull requests (typos, bug fixes and so on) are gladly accepted; for anything larger please raise an issue first.

Issues with the `low-hanging fruit` label should be easy to fix if you want something to get started with.

If you want to start working on an issue (existing or one you just raised), please leave a comment to avoid effort duplication. Issues that someone is already working on are labelled `in progress`.

<!--
* [Triaging our open tickets](TODO)
* [Helping with the current Milestone](TODO)
* Sending an unsolicited pull request with a new feature
* Telling your co-workers!
-->
## See also

[Scalanizer](https://github.com/scalan/scalanizer) - a Scala plugin which allows to capture Scala ASTs and translate it into Scalan.

[Scalanizer Demo](https://github.com/scalan/scalanizer-demo) - a simple project that demonstrates how to use Scalanizer, declare hot-spot regions and generate efficient kernels for JVM and native execution.
