package scalan.compilation.lms.uni

import java.io.File

import scalan._
import scalan.compilation.lms.linalgebra.{LinAlgLmsBridge, LinAlgLmsCompilerUni}
import scalan.graphs.{GraphsDslExp, GraphsDslStd, GraphTestInputs, MsfFuncs}
import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, LADslStd, LADslExp}

/**
 * Created by adel on 5/15/15.
 */

trait UniCompilerTestProg extends MsfFuncs with LinearAlgebraExamples {
  lazy val nop = fun { p: Rep[Double] =>
    p
  }
  lazy val oneOp = fun { p: Rep[Double] =>
    p+2.0
  }

  lazy val mapArray = fun { p: Rep[Array[Double]] =>  //Rep[(Array[Array[Double]], Array[Double])]
    val vector = DenseVector(Collection(p))
    val res = vector.mapBy( fun{ r => r + 2.0 } )
    res.items.arr
  }

  lazy val zipArray = fun {x: Rep[Array[Int]] =>
    val x1 = x.map {y:Rep[Int] => y+2}
    x1 zip x
  }

  lazy val zip2Arrays = fun {x: Rep[(Array[Int], Array[Int])] =>
    val x1 = x._1
    val x2 = x._2
    val x11 = x1.map {y:Rep[Int] => y+2}
    val x21 = x2.map {y:Rep[Int] => y+3}
    x11 zip x21
  }

  lazy val zip3Arrays = fun {x: Rep[(Array[Int], (Array[Int], Array[Int]))] =>
    val x1 = x._1
    val x2 = x._2
    val x3 = x._3
    val x11 = x1.map {y:Rep[Int] => y+1}
    val x21 = x2.map {y:Rep[Int] => y+2}
    val x31 = x3.map {y:Rep[Int] => y+3}
    x31 zip ( x21 zip x11)
  }

  lazy val simpleReduce = fun {x: Rep[Array[Int]] =>
    val x1 = x.reduce
    x1
  }

  lazy val reduceFromTuple = fun {x: Rep[(Array[Int], (Array[Int], Array[Int]))] =>
    val x1 = x._2.reduce
    x1 + 1
  }

  def test_config_f(y: Rep[Int]):Rep[Int] = y+10
  def test_config_g(y: Rep[Int]):Rep[Int] = y*2
  def test_config_h(y: Rep[Int]):Rep[Int] = -y+3
  def test_config_s(y: Rep[Int]):Rep[Int] = y*y

  lazy val test_config = fun {x: Rep[(Array[Int], (Array[Int], Array[Int]))] =>
    //combinations: f or g - sipmplest case
    //              h      - one function 2 times
    //              f & g  - 2 separate functions
    //              h      - one function 2 times
    //              s & g  - superposition of 2 functions
    //              s & h  - superposition of 2 functions, second function called also directly

    val x1 = x._1.map {y:Rep[Int] => test_config_f(y) + 1}.reduce
    val x2 = x._2.map {y:Rep[Int] => test_config_s(test_config_g(y)) + 1}.reduce
    val x3 = x._3.map {y:Rep[Int] => test_config_h(y) + 1}.reduce
    x1 + x2 + test_config_s(test_config_h(x3))
  }

}

class UniCompilerItTests extends BaseItTests[UniCompilerTestProg](new GraphsDslStd with LADslStd with UniCompilerTestProg) with GraphTestInputs {

  val in3Arrays = (Array(2, 3), (Array(1, 4), Array(1, -1)))

  class ProgExp extends GraphsDslExp with LADslExp with UniCompilerTestProg with JNIExtractorOpsExp

  val progStaged = new LinAlgLmsCompilerUni(new ProgExp) with LinAlgLmsBridge

  val defaultCompilers = compilers(progStaged)

  def progStagedWC(config: progStaged.CompilerConfig) = compilers(cwc(progStaged)(config))

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Tuple2(inM, inV)
    compareOutputWithStd(_.ddmvm)(in)
  }

  ignore("msfFunAdjBase") {
    // todo should be checked after fix error #50 -Applying lambda to Array not worked correctly. See test("applyLambda2Array") in LmsSmokeItTests.scala
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val in = (links, (edgeVals, (offs, lens)))

    compareOutputWithStd(_.msfFunAdjBase)(in)
  }

  test("nop") {
    val in = 5.0
    compareOutputWithStd(_.nop)(in)
  }

  test("oneOp") {
    val in = 5.0
    compareOutputWithStd(_.oneOp)(in)
  }

  test("mapArray") {
    val in = Array(2.0, 3.0)
    compareOutputWithStd(_.mapArray)(in)
  }

  test("zipArray") {
    val in = Array(2, 3)
    compareOutputWithStd(_.zipArray)(in)
  }

  test("zip2Arrays") {
    val in = (Array(2, 3), Array(1, 4))
    compareOutputWithStd(_.zip2Arrays)(in)
  }

  test("zip3Arrays") {
    val in = (Array(2, 3), (Array(1, 4), Array(0, 5)))
    compareOutputWithStd(_.zip3Arrays)(in)
  }

  test("simpleReduce") {
    val in = Array(2, 3)
    compareOutputWithStd(_.simpleReduce)(in)
  }

  test("reduceFromTuple") {
    compareOutputWithStd(_.reduceFromTuple)(in3Arrays)
  }

  // ===========================
  // config tests
  // todo refactoring from "jvm vs native alternative" to "choose codegen" in config


  def linesWithNativeDef(file: File):List[String] =
    scala.io.Source.fromFile(file).getLines.filter(_.contains("@native def ")).toList

  test("config_OnlyScala") {
    val in = (Array(2, 3), (Array(1, 4), Array(0, 5)))
    val config = progStaged.defaultCompilerConfig.copy(nativeMethods = new NativeMethodsConfig(rootIsNative = false, Nil))
    val file = compileSource(progStaged)(_.reduceFromTuple, "config_OnlyScala", config).custom.sources.head
    assert(linesWithNativeDef(file).size == 0)
    compareOutputWithStd(_.reduceFromTuple, progStagedWC(config))(in)
  }

  test("config_Root") {
    val in = (Array(2, 3), (Array(1, 4), Array(0, 5)))

    val config = progStaged.defaultCompilerConfig
    val noNative = config.copy(nativeMethods = new NativeMethodsConfig(rootIsNative = false, Nil))
    val nativeRoot = config.copy(nativeMethods = new NativeMethodsConfig(rootIsNative = true))
    val fileJ = compileSource(progStaged)(_.reduceFromTuple, "config_RootJ", noNative).custom.sources.head
    assert(linesWithNativeDef(fileJ).size == 0)
    val fileC = compileSource(progStaged)(_.reduceFromTuple, "config_RootC", nativeRoot).custom.sources.head
    assert(linesWithNativeDef(fileC).size == 1)
  }

  test("config_MethodCall") {
    pending
    // todo make as native MethodCall pointed in config
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Tuple2(inM, inV)

    val config = progStaged.defaultCompilerConfig
    val noNative = config.copy(nativeMethods = new NativeMethodsConfig(rootIsNative = false, Nil))
    val fileJ = compileSource(progStaged)(_.ddmvm, "config_MethodCallJ", noNative).custom.sources.head
    assert(linesWithNativeDef(fileJ).size == 0)
    val nativeMul = config.copy(nativeMethods = new NativeMethodsConfig(rootIsNative = false))   // todo - set matrix.operator*(vector) as native
    val fileC = compileSource(progStaged)(_.ddmvm, "config_MethodCallC", nativeMul).custom.sources.head
    assert(linesWithNativeDef(fileC).size == 1)
    // todo check, that main method is not native
    assert(linesWithNativeDef(fileC).size-100 == 1)

    compareOutputWithStd(_.ddmvm, progStagedWC(nativeMul))(in)
  }

  test("config_MarkedAsNativeLambda") {
    pending
    // todo make as native lambda, marked in code
    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val native = new NativeMethodsConfig(rootIsNative = false)   // todo - set test_config_f as native
    val config = progStaged.defaultCompilerConfig.copy(nativeMethods = native)

    //check sources for two variants

    compareOutputWithStd(_.test_config, progStagedWC(config))(in3Arrays)
  }

  test("config_NamedLambda") {
    pending
    // todo - set test_config_g as native
    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val native = new NativeMethodsConfig(rootIsNative = false)

    //check sources for two variants

  }

  test("config_functions2times") {
    pending
    // todo - set test_config_h as native
    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val native = new NativeMethodsConfig(rootIsNative = false)

    //check sources for two variants

  }

  test("config_2Functions") {
    pending
    // todo - set test_config_f and test_config_g as native
    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val native = new NativeMethodsConfig(rootIsNative = false)

    //check sources for two variants

  }

  test("config_SuperposLambda") {
    pending
    // todo - set test_config_s and test_config_g as native
    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val native = new NativeMethodsConfig(rootIsNative = false)

    //check sources for two variants

  }

  test("config_SuperposLambdafrom2Points") {
    pending
    // todo - set test_config_s and test_config_h as native
    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val native = new NativeMethodsConfig(rootIsNative = false)

    //check sources for two variants
  }

  test("config_SuperposWithMethodCall") {
    pending
    // todo - SuperposWithMethodCall
  }

  test("config_SuperposCheckOptimization") {
    pending
    // todo - check that f(g(x)) not used connvertion of g(x) to scala nnd then from scala
  }

}
