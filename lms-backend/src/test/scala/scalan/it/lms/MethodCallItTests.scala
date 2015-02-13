package scalan.it.lms

import scala.language.reflectiveCalls
import scalan.ScalanCtxExp
import scalan.community.{ScalanCommunityDslExp, ScalanCommunityExp}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.it.BaseItTests
import scalan.it.lms.method.TestMethod
import scalan.linalgebra.MatricesDslExp
import scalan.util.FileUtil
import scalan.util.FileUtil.packJar

class MethodCallItTests extends LmsCommunityItTests{
  trait Prog extends ProgCommunity  {

    lazy val emptyIf = fun { (in: Rep[(Boolean, (Double, Double))] ) =>  {
      val Pair(x, Pair(y, z)) = in
      IF(x) THEN y ELSE z
    }}

    lazy val exceptionWithIfFalse = fun { (p: Rep[(Int, (Throwable, Throwable))]) => {
      val Pair(n, Pair(t1, t2)) = p
      IF(n>0) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }
    }

    lazy val exceptionWithIfTrue = fun { (p: Rep[(Int, (Throwable, Throwable))]) => {
      //val Pair(n, Pair(t1, t2)) = p
      val Pair(n, Pair(t1, t2)) = p
      IF(n<=0) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }
    }

    lazy val arrayLengthFun = fun { (v: Rep[Array[Int]] ) =>  {
      //v.arrayLength
      v.length
    }}

    lazy val arrayOneArg = fun { (in: Rep[(Array[Double], Int)] ) =>  {
      in._1.apply(in._2)
    }}

    lazy val easyMap = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      val m:Rep[Array[Double]] = m0.map(x => x*x)
      m.reduce
    }}

    lazy val mapWithLambda = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] =  {
        x*y+z
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }}

    lazy val mapWithLambdaIf = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] =  {
        IF(x===y) THEN z ELSE x
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }}

    lazy val mapWithLambdaIfGt = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] =  {
        IF(x<y) THEN x ELSE z
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }}
  }

  class ProgSeq extends ProgCommunitySeq with Prog  {}
  class ProgStaged extends ProgCommunityExp with  Prog {}

  override val progSeq = new ProgSeq
  override val progStaged = new ProgStaged



  test("emptyIfTrue") {
    val in = (true, (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.emptyIf, progStaged.emptyIf, "emptyIfTrue", in)
  }

  test("emptyIfFalse") {
    val in = (false, (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.emptyIf, progStaged.emptyIf, "emptyIfFalse", in)
  }

  ignore("exceptionWithIfTrue") {
    //val in = Array(Array(2, 3), Array(4, 5))
    val in = (1, (new Exception("branch true exception"), new Exception("branch false exception") ))
    compareOutputWithSequential(progStaged)(progSeq.exceptionWithIfTrue, progStaged.exceptionWithIfTrue, "exceptionWithIfTrue", in)
  }

  test("exceptionWithIfFalse") {
    //val in = Array(Array(2, 3), Array(4, 5))
    val in = (1, (new Exception("branch true exception"), new Exception("branch false exception") ))
    compareOutputWithSequential(progStaged)(progSeq.exceptionWithIfFalse, progStaged.exceptionWithIfFalse, "exceptionWithIfFalse", in)
  }

  test("arrayLengthFun") {
    val in = Array(4, 5, 6)
    compareOutputWithSequential(progStaged)(progSeq.arrayLengthFun, progStaged.arrayLengthFun, "arrayLengthFun", in)
  }

  test("arrayOneArg") {
    val in = (Array(4.4, 5.0, 6.1), 2)
    compareOutputWithSequential(progStaged)(progSeq.arrayOneArg, progStaged.arrayOneArg, "arrayOneArg", in)
  }

  test("easyMap") {
    val in = (Array(4.4, 5.0, 6.1), (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.easyMap, progStaged.easyMap, "easyMap", in)
  }

  test("mapWithLambda") {
    val in = (Array(4.4, 5.0, 6.1), (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.mapWithLambda, progStaged.mapWithLambda, "mapWithLambda", in)
  }

  test("mapWithLambdaIf") {
    val in = (Array(4.4, 5.0, 6.1), (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.mapWithLambdaIf, progStaged.mapWithLambdaIf, "mapWithLambdaIf", in)
  }

  test("mapWithLambdaIfGt") {
    val in = (Array(4.4, 5.0, 6.1), (5.5, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.mapWithLambdaIfGt, progStaged.mapWithLambdaIfGt, "mapWithLambdaIfGt", in)
  }

}

class MethodCallItTestsOld extends BaseItTests {

  trait TestLmsCompiler extends ScalanCommunityDslExp with ScalanCtxExp with LmsCompilerScala with CommunityBridge {
    val lms = new CommunityLmsBackend
  }

  val exceptionTestExp = new ScalanCommunityExp with TestLmsCompiler {
    self =>

    lazy val tElem = element[Throwable]
    lazy val defaultRep = tElem.defaultRepValue

    lazy val message = fun { (t: Rep[Throwable]) => t.getMessage}

    lazy val initCause = fun { (p: Rep[(Throwable, Throwable)]) => {
      val Pair(t1, t2) = p
      t1.initCause(t2).getMessage
    }
    }

    lazy val initCause2 = fun { (p: Rep[(Throwable, (Throwable, (Throwable, (Throwable, Throwable))))]) => {
      val Pair(t1, Pair(t2, Pair(t3, Pair(t4, t5)))) = p
      t1.initCause(t2.initCause(t3.initCause(t4.initCause(t5)))).getMessage
    }
    }

    lazy val withIfFalse = fun { (p: Rep[(Throwable, Throwable)]) => {
      val Pair(t1, t2) = p
      IF(false) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }
    }

    lazy val withIfTrue = fun { (p: Rep[(Throwable, Throwable)]) => {
      val Pair(t1, t2) = p
      IF(true) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }
    }
  }

  test("Throwable Method Call") {
    val originalText = "Exception massage"
    val text = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.message, "ThrowableMethodCall", new Exception(originalText), exceptionTestExp.defaultCompilerConfig)
    text should equal(originalText)
    val text2 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.initCause, "ThrowableInitCauseMethodCall", (new Exception(originalText), new Exception("some text")), exceptionTestExp.defaultCompilerConfig)
    text2 should equal(originalText)
    val text3 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.initCause2, "ThrowableManyInitCauseMethodCall", (new Exception(originalText), (new Exception("some text"), (new Exception("some text"), (new Exception("some text"), new Exception("some text"))))), exceptionTestExp.defaultCompilerConfig)
    text3 should equal(originalText)
    val text4 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.withIfFalse, "IfFalseMethodCall", (new Exception("Exception massage"), new Exception("some text")), exceptionTestExp.defaultCompilerConfig)
    text4 should equal(originalText)
    val text5 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.withIfTrue, "IfTrueMethodCall", (new Exception("Exception massage"), new Exception("some text")), exceptionTestExp.defaultCompilerConfig)
    text5 should equal("some text")
  }

  val matricesExp = new MatricesDslExp with ScalanCommunityExp with TestLmsCompiler {
    self =>

    lazy val arrayLength = fun { v: Rep[Array[Int]] =>
      PArray(v).length
    }
  }

  test("LMS Method Call") {
    val length = getStagedOutputConfig(matricesExp)(matricesExp.arrayLength, "LMSMethodCall", Array(2, 5, 6), matricesExp.defaultCompilerConfig)
    length should equal(3)
  }

  trait ReplacementExp extends MatricesDslExp with ScalanCommunityExp with TestLmsCompiler {
    lazy val arrayLength = fun { v: Rep[Array[Int]] =>
      PArray(v).length
    }
  }

  val replaceMethExp = new ReplacementExp {
    self =>

    new ScalaLanguage with CommunityConf {
      val javaArray = new ScalaLib("", "java.lang.reflect.Array") {
        val getLength = ScalaFunc('getLength)
      }

      val scala2Scala = {
        import scala.language.reflectiveCalls

        Map(
          scalanCE.parraysPack.parraysFam.parray.length -> javaArray.getLength
        )
      }

      val backend = new ScalaBackend {
        val functionMap = scala2Scala
      }
    }
  }

  test("Method Replacement") {
    val length = getStagedOutputConfig(replaceMethExp)(replaceMethExp.arrayLength, "MethodReplacement", Array(5, 9, 2), replaceMethExp.defaultCompilerConfig)
    length should equal(3)
  }

  val testJar = "test.jar"
  val jarReplaceMethExp = new ReplacementExp {
    self =>

    new ScalaLanguage with CommunityConf {

      val extLib = new ScalaLib(testJar, "scalan.it.lms.method.TestMethod") {
        val getSquareLength = ScalaFunc('getSquareLength)
      }

      val scala2Scala = {
        import scala.language.reflectiveCalls

        Map(
          scalanCE.parraysPack.parraysFam.parray.length -> extLib.getSquareLength
        )
      }

      val backend = new ScalaBackend {
        val functionMap = scala2Scala
      }
    }
  }

  ignore("Mapping Method From Jar") {
    val methodName = "MappingMethodFromJar"
    packJar(TestMethod.getClass, methodName, FileUtil.file(prefix, methodName).getAbsolutePath, jarReplaceMethExp.libs, testJar)
    val squareLength = getStagedOutputConfig(jarReplaceMethExp)(jarReplaceMethExp.arrayLength, methodName, Array(5, 9, 2), jarReplaceMethExp.defaultCompilerConfig)
    squareLength should equal(9)
  }
}


