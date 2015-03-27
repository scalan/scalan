package scalan

import scala.collection.mutable
import scalan.compilation.language.LanguageId._
import scalan.compilation.language._

class MethodMappingTest extends BaseTests {

  object TestMethodMapping extends MethodMapping {

    trait CommunityConf extends LanguageConf {

      val scalanCE = new Library("scalan-ce.jar") {

        val collectionsPack = new Pack("scalan.collections") {
          val collectionsFam = new Family('Collections) {
            val collection = new ClassType('Collection, 'Coll, TyArg('A)) {
              val length = Method('length, tyInt)
            }
          }
        }
      }
    }

    new ScalaLanguage with CommunityConf {
      import scala.language.reflectiveCalls

      val linpackScala = new ScalaLib("linpack.jar", "org.linpack") {
        val invertMatr = ScalaFunc('invertMatrixDouble, ScalaArg(ScalaType('lapack_matr), 'm), ScalaArg(ScalaType('T1), 'V1))()
        val arrayLength = ScalaFunc('arrayLength)()
      }

      val mapScalanCE2Scala = {
        import scalanCE._
        mutable.Map(
          collectionsPack.collectionsFam.collection.length -> linpackScala.arrayLength
        )
      }

      val backend = new ScalaBackend {
        val functionMap = mapScalanCE2Scala // ++ ???
      }
    }

    new CppLanguage with CommunityConf {
      import scala.language.reflectiveCalls

      val linpackCpp = new CppLib("linpack.h", "linpack.o") {
        val invertMatr = CppFunc("invertMatrix", CppArg(CppType("lapack_matr"), "m"), CppArg(CppType("T2"), "V2"))
        val transMatr = CppFunc("transMatrixDouble")
      }

      val mapScalanCE2Cpp = {
        import scalanCE._
        mutable.Map(
          collectionsPack.collectionsFam.collection.length -> linpackCpp.invertMatr
        )
      }

      val backend = new CppBackend {
        val functionMap = mapScalanCE2Cpp // ++ ???
      }
    }
  }

  test("Scala Method") {
    val m = TestMethodMapping.methodReplaceConf.head.get("scalan.collections.Collections$Collection", "length").get.asInstanceOf[MethodMapping#ScalaLanguage#ScalaFunc]
    "arrayLength" should equal(m.funcName.name)
    m.args.size should equal(0)
  }

  test("C++ Method") {
    implicit def defaultLanguage: LANGUAGE = CPP
    val m = TestMethodMapping.methodReplaceConf.head.get("scalan.collections.Collections$Collection", "length").get.asInstanceOf[MethodMapping#CppLanguage#CppFunc]
    "invertMatrix" should equal(m.funcName)
    m.args.size should equal(2)
  }
}