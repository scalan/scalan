package scalan.it.lms

import scalan.graphs.{GraphsDslExp, GraphsDslSeq}
import scalan.linalgebra.{MatricesDslSeq, LinearAlgebraExamples}
import scalan.{ScalanCtxExp, ScalanCommunityDslSeq, CommunityMethodMappingDSL, ScalanCommunityDslExp}
import scalan.compilation.lms.uni.{NativeMethodsConfig, LmsCompilerUni}
import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridge}

/**
 * Created by adel on 5/15/15.
 */

class UniCompilerItTests  extends LmsMsfItTests {

  //trait ProgUniTest extends ProgCommunity with MsfFuncs with LinearAlgebraExamples with CommunityMethodMappingDSL {
  trait ProgUniTest extends MsfFuncs with LinearAlgebraExamples with CommunityMethodMappingDSL {
    lazy val test00_nop = fun { p: Rep[Double] =>
      p
    }
    lazy val test01_oneOp = fun { p: Rep[Double] =>
      p+2.0
    }

    lazy val test02_mapArray = fun { p: Rep[Array[Double]] =>  //Rep[(Array[Array[Double]], Array[Double])]
      val vector = DenseVector(Collection(p))
      val res = vector.mapBy( fun{ r => r + 2.0 } )
      res.items.arr
    }

    lazy val test03_zipArray = fun {x: Rep[Array[Int]] =>
      val x1 = x.map {y:Rep[Int] => y+2}
      x1 zip x
    }

    lazy val test04_zip2Arrays = fun {x: Rep[(Array[Int], Array[Int])] =>
      val x1 = x._1
      val x2 = x._2
      val x11 = x1.map {y:Rep[Int] => y+2}
      val x21 = x2.map {y:Rep[Int] => y+3}
      x11 zip x21
    }

    lazy val test05_zip3Arrays = fun {x: Rep[(Array[Int], (Array[Int], Array[Int]))] =>
      val x1 = x._1
      val x2 = x._2
      val x3 = x._3
      val x11 = x1.map {y:Rep[Int] => y+1}
      val x21 = x2.map {y:Rep[Int] => y+2}
      val x31 = x3.map {y:Rep[Int] => y+3}
      x31 zip ( x21 zip x11)
    }

    lazy val test06_simpleReduce = fun {x: Rep[Array[Int]] =>
      val x1 = x.reduce
      x1
    }

    lazy val test07_reduceFromTuple = fun {x: Rep[(Array[Int], (Array[Int], Array[Int]))] =>
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

      val x1 = x._1.map {y:Rep[Int] => test_config_f(y) + 1} . reduce
      val x2 = x._2.map {y:Rep[Int] => test_config_s(test_config_g(y)) + 1} . reduce
      val x3 = x._3.map {y:Rep[Int] => test_config_h(y) + 1} . reduce
      x1 + x2 + test_config_s(test_config_h(x3))
    }

  }

  val in3Arrays = (Array(2, 3), (Array(1, 4), Array(1, -1)))

                 //ProgExp extends CommunityLmsCompilerScala with CommunityBridge
  class ProgCommunityExp extends ProgUniTest with GraphsDslExp with ScalanCtxExp with ScalanCommunityDslExp with LmsCompilerUni with CommunityBridge

  val progStaged = new ProgCommunityExp

  class ProgSeq extends ProgUniTest with GraphsDslSeq with MatricesDslSeq with ScalanCommunityDslSeq

  val progSeqU = new ProgSeq


  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Tuple2(inM, inV)
    compareOutputWithSequential(progStaged)(progSeqU.ddmvm, progStaged.ddmvm, "ddmvm00", in)
  }

  ignore("msfFunAdjBase") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))

    compareOutputWithSequential(progStaged)(progSeqU.msfFunAdjBase, progStaged.msfFunAdjBase, "msfFunAdjBase", input)
  }

  test("test00_nop") {
    val in = 5.0
    compareOutputWithSequential(progStaged)(progSeqU.test00_nop, progStaged.test00_nop, "test00_nop", in)
  }

  test("test01_oneOp") {
    val in = 5.0
    compareOutputWithSequential(progStaged)(progSeqU.test01_oneOp, progStaged.test01_oneOp, "test01_oneOp", in)
  }

  test("test02_mapArray") {
    val in = Array(2.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeqU.test02_mapArray, progStaged.test02_mapArray, "test02_mapArray", in)
  }

  test("test03_zipArray") {
    val in = Array(2, 3)
    compareOutputWithSequential(progStaged)(progSeqU.test03_zipArray, progStaged.test03_zipArray, "test03_zipArray", in)
  }

  test("test04_zip2Arrays") {
    val in = (Array(2, 3), Array(1, 4))
    compareOutputWithSequential(progStaged)(progSeqU.test04_zip2Arrays, progStaged.test04_zip2Arrays, "test04_zip2Arrays", in)
  }

  test("test05_zip3Arrays") {
    val in = (Array(2, 3), (Array(1, 4), Array(0, 5)))
    compareOutputWithSequential(progStaged)(progSeqU.test05_zip3Arrays, progStaged.test05_zip3Arrays, "test05_zip3Arrays", in)
  }

  test("test06_simpleReduce") {
    val in = Array(2, 3)
    compareOutputWithSequential(progStaged)(progSeqU.test06_simpleReduce, progStaged.test06_simpleReduce, "test06_simpleReduce", in)
  }

  test("test07_reduceFromTuple") {
    compareOutputWithSequential(progStaged)(progSeqU.test07_reduceFromTuple, progStaged.test07_reduceFromTuple, "test07_reduceFromTuple", in3Arrays)
  }

  // ===========================
  // config tests
  // todo refactoring from "jvm vs native alternative" to "choose codegen" in config


  def linesWithNativeDef(fileName:String):List[String] =
    scala.io.Source.fromFile(fileName).getLines.filter(_.contains("@native def ")).toList

  test("config_OnlyScala") {
    val in = (Array(2, 3), (Array(1, 4), Array(0, 5)))
    val config = progStaged.defaultCompilerConfig.copy(nativeMethods = new NativeMethodsConfig(rootIsNative = false, Nil))
    val file = compileSource(progStaged)(progStaged.test07_reduceFromTuple, "config_OnlyScala", config).custom.sources.head
    assert(linesWithNativeDef(file).size == 0)
    compareOutputWithSequentialConfig(progStaged)(progSeqU.test07_reduceFromTuple, progStaged.test07_reduceFromTuple, "config_OnlyScala", in, config)
  }

  test("config_Root") {
    val in = (Array(2, 3), (Array(1, 4), Array(0, 5)))

    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val nativeRoot = new NativeMethodsConfig(rootIsNative = true)
    val config = progStaged.defaultCompilerConfig
    val fileJ = compileSource(progStaged)(progStaged.test07_reduceFromTuple, "config_RootJ", config.copy(nativeMethods = noNative)).custom.sources.head
    assert(linesWithNativeDef(fileJ).size == 0)
    val fileC = compileSource(progStaged)(progStaged.test07_reduceFromTuple, "config_RootC", config.copy(nativeMethods = nativeRoot)).custom.sources.head
    assert(linesWithNativeDef(fileC).size == 1)
  }

  test("config_MethodCall") {
    pending
    // todo make as native MethodCall pointed in config
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Tuple2(inM, inV)

    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val nativeMul = new NativeMethodsConfig(rootIsNative = false)   // todo - set matrix.operator*(vector) as native
    val config = progStaged.defaultCompilerConfig
    val fileJ = compileSource(progStaged)(progStaged.ddmvm, "config_MethodCallJ", config.copy(nativeMethods = noNative)).custom.sources.head
    assert(linesWithNativeDef(fileJ).size == 0)
    val fileC = compileSource(progStaged)(progStaged.ddmvm, "config_MethodCallC", config.copy(nativeMethods = nativeMul)).custom.sources.head
    assert(linesWithNativeDef(fileC).size == 1)
    // todo check, that main method is not native
    assert(linesWithNativeDef(fileC).size-100 == 1)

    compareOutputWithSequentialConfig(progStaged)(progSeqU.ddmvm, progStaged.ddmvm, "config_MethodCall", in, config.copy(nativeMethods = nativeMul))

  }

  test("config_MarkedAsNativeLambda") {
    pending
    // todo make as native lambda, marked in code
    val noNative = new NativeMethodsConfig(rootIsNative = false, Nil)
    val native = new NativeMethodsConfig(rootIsNative = false)   // todo - set test_config_f as native
    val config = progStaged.defaultCompilerConfig

    //check sources for two variants

    compareOutputWithSequentialConfig(progStaged)(progSeqU.test_config, progStaged.test_config, "config_MarkedAsNativeLambda", in3Arrays, config.copy(nativeMethods = native))
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

