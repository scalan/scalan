package scalan

import scalan.compilation.language.LanguageId._
import scalan.compilation.language._

class MethodMappingDSLTest extends BaseTests {

  object TestMethodMappingDSL$ extends MethodMappingDSL {

    trait CommunityConf extends MappingTags {

      val scalanCE = new Library("scalan-ce.jar") {

        val collectionsPack = new Pack("scalan.collections") {
          val collectionsFam = new Family('Collections) {
            val collection = new ClassType('Collection, TyArg('A)) {
              val length = Method('length, tyInt)
            }
          }
        }
      }
    }

    new ScalaMappingDSL with CommunityConf {
      import scala.language.reflectiveCalls

      val linpackScala = new ScalaLib("linpack.jar", "org.linpack") {
        val invertMatr = ScalaFunc('invertMatrixDouble, ScalaArg(ScalaType('lapack_matr), 'm), ScalaArg(ScalaType('T1), 'V1))()
        val arrayLength = ScalaFunc('arrayLength)()
      }

      val mapScalanCE2Scala = {
        import scalanCE._
        Map(
          collectionsPack.collectionsFam.collection.length -> linpackScala.arrayLength
        )
      }

      val mapping = new ScalaMapping {
        val functionMap = mapScalanCE2Scala
      }
    }

    new CppMappingDSL with CommunityConf {
      import scala.language.reflectiveCalls

      val linpackCpp = new CppLib("linpack.h", "linpack.o") {
        val invertMatr = CppFunc("invertMatrix", CppArg(CppType("lapack_matr"), "m"), CppArg(CppType("T2"), "V2"))
        val transMatr = CppFunc("transMatrixDouble")
      }

      val mapScalanCE2Cpp = {
        import scalanCE._
        Map(
          collectionsPack.collectionsFam.collection.length -> linpackCpp.invertMatr
        )
      }

      val mapping = new CppMapping {
        val functionMap = mapScalanCE2Cpp
      }
    }
  }

  test("Scala Method") {
    val m = TestMethodMappingDSL$.methodReplaceConf.head.get("scalan.collections.Collections$Collection", "length").get.asInstanceOf[MethodMappingDSL#ScalaMappingDSL#ScalaFunc]
    "arrayLength" should equal(m.name)
    m.args.size should equal(0)
  }

  test("C++ Method") {
    implicit def defaultLanguage: LANGUAGE_ID = CPP
    val m = TestMethodMappingDSL$.methodReplaceConf.head.get("scalan.collections.Collections$Collection", "length").get.asInstanceOf[MethodMappingDSL#CppMappingDSL#CppFunc]
    "invertMatrix" should equal(m.name)
    m.args.size should equal(2)
  }
}