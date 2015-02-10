package scalan.it.lms

import java.io.File
import java.io.File.{separator => s}

import scala.language.reflectiveCalls
import scalan.community.{ScalanCommunityDslExp, ScalanCommunityExp}
import scalan.compilation.GraphVizExport
import scalan.compilation.language.Library
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.it.BaseItTests
import scalan.it.lms.method.TestMethod
import scalan.{ScalanCtxExp, ScalanExp}

import scalan.linalgebra.{Matrices, MatricesDslExp}
import scalan.util.ProcessUtil.launch

class MethodCallItTests extends BaseItTests {

  trait TestLmsCompiler extends ScalanCommunityDslExp with ScalanCtxExp with LmsCompilerScala {
    self: ScalanExp with GraphVizExport =>
  }

  val exceptionTestExp = new ScalanCommunityExp with TestLmsCompiler {
    self =>
    def makeBridge[A, B] = new CommunityBridge[A, B] {
      val scalan = self
      override val lms = new CommunityLmsBackend
    }

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
    def makeBridge[A, B] = new CommunityBridge[A, B] {
      val scalan = self
      val lms = new CommunityLmsBackend
    }

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
    def makeBridge[A, B] = new CommunityBridge[A, B] {
      val scalan = self
      override val lms = new CommunityLmsBackend

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
  }

  test("Method Replacement") {
    val length = getStagedOutputConfig(replaceMethExp)(replaceMethExp.arrayLength, "MethodReplacement", Array(5, 9, 2), replaceMethExp.defaultCompilerConfig)
    length should equal(3)
  }

  val testJar = "test.jar"
  val jarReplaceMethExp = new ReplacementExp {
    self =>
    def makeBridge[A, B] = new CoreBridge[A, B] {
      val scalan = self
      override val lms = new CommunityLmsBackend

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
  }

  private def packJar(baseClass: Class[_], methodName: String) = {
    new File(s"${executableDir(methodName).getAbsolutePath}$s${jarReplaceMethExp.libs}$s").mkdirs()
    launch(new File(baseClass.getClassLoader.getResource(".").toURI), Seq("jar", "-cvf", s"${executableDir(methodName).getAbsolutePath}$s${jarReplaceMethExp.libs}$s$testJar") :+ baseClass.getPackage.getName.replaceAll("\\.", s): _*)
  }

  ignore("Mapping Method From Jar") {
    val methodName = "MappingMethodFromJar"
    packJar(TestMethod.getClass, methodName)
    val squareLength = getStagedOutputConfig(jarReplaceMethExp)(jarReplaceMethExp.arrayLength, methodName, Array(5, 9, 2), jarReplaceMethExp.defaultCompilerConfig)
    squareLength should equal(9)
  }
}


