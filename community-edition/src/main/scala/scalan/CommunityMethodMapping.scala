package scalan

import scala.collection.mutable
import scalan.compilation.language._
import scalan.linalgebra.Matrices

trait CommunityMethodMapping extends MethodMapping {

  trait CommunityConf extends LanguageConf {

    import scala.reflect.runtime.universe.typeOf

    val tyMatrix = typeOf[Matrices#AbstractMatrix[_]]

    val scalanCE = new Library(/*"scalan-ce.jar"*/) {

      val collectionsPack = new Pack("scalan.collections") {
        val collectionsFam = new Family('Collections) {
          val collection = new ClassType('Collection, 'Coll, TyArg('A)) {
            val length = Method('length, tyInt)
            val arr = Method('arr, tyArray)
          }
        }
      }
      val matrixPack = new Pack("scalan.linalgebra") {
        val matrixFam = new Family('Matrices) {
          val matrix = new ClassType('AbstractMatrix, 'PA, TyArg('A)) {
            val invert = Method('invert, tyMatrix)
          }
        }
      }
    }

    val expBaseCollection = new CaseClassObject(typeOf[scalan.collections.impl.CollectionsExp#ExpBaseCollection[_]])
  }

  new ScalaLanguage with CommunityConf {

    val linpackScala = new ScalaLib(/*"linpack.jar",*/ pack = "org.linpack") {
      val invertMatr = ScalaFunc('invertMatrixDouble, ScalaArg(ScalaType('lapack_matr), 'm), ScalaArg(ScalaType('T1), 'V1))()
    }

    val lms = new EmbeddedObject("lms") {
      val arrayLength = ScalaFunc('arrayLength)()
    }

    val mapScalanCE2Scala = {
      import scalanCE._
      import scala.language.reflectiveCalls

      mutable.Map[Method, ScalaFunc](
//        collectionsPack.collectionsFam.collection.length -> lms.arrayLength,
//        matrixPack.matrixFam.matrix.invert -> linpackScala.invertMatr
      )
    }

    val backend = new ScalaBackend {
      val functionMap = mapScalanCE2Scala
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
        matrixPack.matrixFam.matrix.invert -> linpackCpp.invertMatr
      )
    }

    val backend = new CppBackend {
      val functionMap = mapScalanCE2Cpp // ++ ???
    }
  }
}

