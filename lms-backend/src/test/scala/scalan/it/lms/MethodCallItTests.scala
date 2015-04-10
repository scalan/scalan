package scalan.it.lms

import org.scalatest.BeforeAndAfterAll

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.linalgebra.MatricesDslExp
import scalan.util.FileUtil
import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityExp, ScalanCtxExp}

class MethodCallItTests extends LmsCommunityItTests with BeforeAndAfterAll{

  override def beforeAll() = {
    FileUtil.deleteIfExist(prefix)
  }

  trait Prog extends ProgCommunity  {

    lazy val emptyIf = fun { (in: Rep[(Boolean, (Double, Double))]) =>
      val Pair(x, Pair(y, z)) = in
      IF(x) THEN y ELSE z
    }

    lazy val exceptionWithIfFalse = fun { (p: Rep[(Int, (SThrowable, SThrowable))]) =>
      val Pair(n, Pair(t1, t2)) = p
      IF(n>0) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }

    lazy val exceptionWithIfTrue = fun { (p: Rep[(Int, (SThrowable, SThrowable))]) =>
      //val Pair(n, Pair(t1, t2)) = p
      val Pair(n, Pair(t1, t2)) = p
      IF(n<=0) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }

    lazy val arrayLengthFun = fun { (v: Rep[Array[Int]]) =>
      //v.arrayLength
      v.length
    }

    lazy val arrayOneArg = fun { (in: Rep[(Array[Double], Int)]) =>
      in._1.apply(in._2)
    }

    lazy val easyMap = fun { (in: Rep[(Array[Double], (Double, Double))]) =>
      val Pair(m0, Pair(vFrom, vTo)) = in
      val m:Rep[Array[Double]] = m0.map(x => x*x)
      m.reduce
    }

    lazy val mapWithLambda = fun { (in: Rep[(Array[Double], (Double, Double))]) =>
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] = {
        x*y+z
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }

    lazy val mapWithLambdaIf = fun { (in: Rep[(Array[Double], (Double, Double))]) =>
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] = {
        IF(x===y) THEN z ELSE x
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }

    lazy val mapWithLambdaIfGt = fun { (in: Rep[(Array[Double], (Double, Double))]) =>
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] = {
        IF(x<y) THEN x ELSE z
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }
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

//  ignore("exceptionWithIfTrue") {
//    //val in = Array(Array(2, 3), Array(4, 5))
//    import progSeq._
//    val in = (1, (SThrowable("branch true exception"), SThrowable("branch false exception") ))
//    compareOutputWithSequential(progStaged)(progSeq.exceptionWithIfTrue, progStaged.exceptionWithIfTrue, "exceptionWithIfTrue", in)
//  }
//
//  test("exceptionWithIfFalse") {
//    //val in = Array(Array(2, 3), Array(4, 5))
//    val in = (1, (new Exception("branch true exception"), new Exception("branch false exception") ))
//    compareOutputWithSequential(progStaged)(progSeq.exceptionWithIfFalse, progStaged.exceptionWithIfFalse, "exceptionWithIfFalse", in)
//  }
//
//  test("arrayLengthFun") {
//    val in = Array(4, 5, 6)
//    compareOutputWithSequential(progStaged)(progSeq.arrayLengthFun, progStaged.arrayLengthFun, "arrayLengthFun", in)
//  }
//
//  test("arrayOneArg") {
//    val in = (Array(4.4, 5.0, 6.1), 2)
//    compareOutputWithSequential(progStaged)(progSeq.arrayOneArg, progStaged.arrayOneArg, "arrayOneArg", in)
//  }
//
//  test("easyMap") {
//    val in = (Array(4.4, 5.0, 6.1), (5.0, 7.7))
//    compareOutputWithSequential(progStaged)(progSeq.easyMap, progStaged.easyMap, "easyMap", in)
//  }
//
//  test("mapWithLambda") {
//    val in = (Array(4.4, 5.0, 6.1), (5.0, 7.7))
//    compareOutputWithSequential(progStaged)(progSeq.mapWithLambda, progStaged.mapWithLambda, "mapWithLambda", in)
//  }
//
//  test("mapWithLambdaIf") {
//    val in = (Array(4.4, 5.0, 6.1), (5.0, 7.7))
//    compareOutputWithSequential(progStaged)(progSeq.mapWithLambdaIf, progStaged.mapWithLambdaIf, "mapWithLambdaIf", in)
//  }
//
//  test("mapWithLambdaIfGt") {
//    val in = (Array(4.4, 5.0, 6.1), (5.5, 7.7))
//    compareOutputWithSequential(progStaged)(progSeq.mapWithLambdaIfGt, progStaged.mapWithLambdaIfGt, "mapWithLambdaIfGt", in)
//  }

  trait TestLmsCompiler extends ScalanCommunityDslExp with ScalanCtxExp with CommunityLmsCompilerScala with CommunityBridge {
    val lms = new CommunityLmsBackend
  }

  val exceptionTestExp = new ScalanCommunityExp with TestLmsCompiler {
    self =>

    lazy val tElem = element[Throwable]
    lazy val defaultRep = tElem.defaultRepValue

    lazy val message = fun { (t: Rep[SThrowable]) => t.getMessage}

    lazy val initCause = fun { (p: Rep[(SThrowable, SThrowable)]) =>
      val Pair(t1, t2) = p
      t1.initCause(t2).getMessage
    }

    lazy val initCause2 = fun { (p: Rep[(SThrowable, (SThrowable, (SThrowable, (SThrowable, SThrowable))))]) =>
      val Pair(t1, Pair(t2, Pair(t3, Pair(t4, t5)))) = p
      t1.initCause(t2.initCause(t3.initCause(t4.initCause(t5)))).getMessage
    }

    lazy val withIfFalse = fun { (p: Rep[(SThrowable, SThrowable)]) =>
      val Pair(t1, t2) = p
      IF(false) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }

    lazy val withIfTrue = fun { (p: Rep[(SThrowable, SThrowable)]) =>
      val Pair(t1, t2) = p
      IF(true) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }
  }

//  test("Throwable Method Call") {
//    val originalText = "Exception message"
//    val text = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.message, "ThrowableMethodCall", new Exception(originalText), exceptionTestExp.defaultCompilerConfig)
//    text should equal(originalText)
//    val text2 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.initCause, "ThrowableInitCauseMethodCall", (new Exception(originalText), new Exception("some text")), exceptionTestExp.defaultCompilerConfig)
//    text2 should equal(originalText)
//    val text3 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.initCause2, "ThrowableManyInitCauseMethodCall", (new Exception(originalText), (new Exception("some text"), (new Exception("some text"), (new Exception("some text"), new Exception("some text"))))), exceptionTestExp.defaultCompilerConfig)
//    text3 should equal(originalText)
//    val text4 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.withIfFalse, "IfFalseMethodCall", (new Exception("Exception message"), new Exception("some text")), exceptionTestExp.defaultCompilerConfig)
//    text4 should equal(originalText)
//    val text5 = getStagedOutputConfig(exceptionTestExp)(exceptionTestExp.withIfTrue, "IfTrueMethodCall", (new Exception("Exception message"), new Exception("some text")), exceptionTestExp.defaultCompilerConfig)
//    text5 should equal("some text")
//  }

  val matricesExp = new MatricesDslExp with ScalanCommunityExp with TestLmsCompiler {
    self =>

    lazy val arrayLength = fun { v: Rep[Array[Int]] =>
      Collection(v).length
    }
  }

  test("LMS Method Call") {
    val length = getStagedOutputConfig(matricesExp)(matricesExp.arrayLength, "LMSMethodCall", Array(2, 5, 6), matricesExp.defaultCompilerConfig)
    length should equal(3)
  }

  val replaceMethExp = new MatricesDslExp with ScalanCommunityExp with TestLmsCompiler with CommunityMethodMappingDSL {
    lazy val arrayLength = fun { v: Rep[Array[Int]] =>
      Collection(v).length
    }

    override def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, invokeEnabler("skip_length_method") { (o, m) => !m.getName.equals("length")})
  }

  test("Class Mapping") {
    if (isOnTeamCity) {
      // takes extremely long to run on TeamCity
      pending
    }
    val conf = replaceMethExp.defaultCompilerConfig
    val length = getStagedOutputConfig(replaceMethExp)(replaceMethExp.arrayLength, "ClassMapping", Array(5, 9, 2),
      conf.copy(scalaVersion = Some("2.11.6"), sbt = conf.sbt.copy(mainPack = Some("scalan.imp"),
        extraClasses = Seq("scalan.imp.ArrayImp"), commands = Seq("package"))))
    length should equal(3)
  }

  val jarReplaceExp = new ScalanCommunityExp with TestLmsCompiler {
    self =>

    lazy val message = fun { (t: Rep[String]) => SThrowable(t).getMessage}

    import scala.reflect.runtime.universe.typeOf
    val tyThrowable = typeOf[Throwable]

    trait TestConf extends MappingTags {
      val testLib = new Library("") {
        val scalanUtilPack = new Pack("scalan.util") {
          val exceptionsFam = new Family('Exceptions) {
            val throwable = new ClassType('SThrowable) {
              val getMessage = Method('getMessage, tyString, MethodArg(tyString))
            }
          }
        }
      }
    }

    new ScalaMappingDSL with TestConf {

      val extLib = new ScalaLib("", "scalan.it.lms.MappingMethodFromJar.TestMethod") {
        val testMessageMethod = ScalaFunc('testMessage)(true)
      }

      val scala2Scala = {
        import scala.language.reflectiveCalls

        Map(
          testLib.scalanUtilPack.exceptionsFam.throwable.getMessage -> extLib.testMessageMethod
        )
      }

      val main = new ScalaLib() {
        val throwableImp = ScalaFunc(Symbol("scalan.imp.ThrowableImp"))(true)
      }

      val mapping = new ScalaMapping {
        val functionMap = scala2Scala
        override val classMap = Map[Class[_], ScalaFunc](classOf[scalan.util.Exceptions#SThrowable] -> main.throwableImp)
      }
    }
  }

  test("Mapping Method From Jar") {
    if (isOnTeamCity) {
      // takes extremely long to run on TeamCity
      pending
    }
    val conf = jarReplaceExp.defaultCompilerConfig
    val messageFromTestMethod = getStagedOutputConfig(jarReplaceExp)(jarReplaceExp.message, "MappingMethodFromJar", "Original message",
      conf.copy(scalaVersion = Some("2.11.6"), sbt = conf.sbt.copy(mainPack = Some("scalan.imp"),
        extraClasses = Seq("scalan.imp.ThrowableImp", "scalan.it.lms.MappingMethodFromJar.TestMethod"), commands = Seq("package"))))
    messageFromTestMethod should equal("Test Message")
  }
}
