package scalan

import scalan.compilation.language._
import scalan.linalgebra.Matrices

trait CommunityMethodMappingDSL extends MethodMappingDSL {

  trait CommunityMappingTags extends MappingTags {

    import scala.reflect.runtime.universe.typeOf

    val tyMatrix = typeOf[Matrices#AbstractMatrix[_]]

    val scalanCE = new Library() {

      val collectionsPack = new Pack("scalan.collections") {
        val collectionsFam = new Family('Collections) {
          val collection = new ClassType('Collection, TyArg('A)) {
            val length = Method('length, tyInt)
            val arr = Method('arr, tyArray)
          }
        }
      }
      val matrixPack = new Pack("scalan.linalgebra") {
        val matrixFam = new Family('Matrices) {
          val matrix = new ClassType('AbstractMatrix, TyArg('A)) {
            val invert = Method('invert, tyMatrix)
          }
        }
      }
    }

    val expCollectionOverArray = new CaseClassObject(typeOf[scalan.collections.impl.CollectionsExp#ExpCollectionOverArray[_]])
  }

  new ScalaMappingDSL with CommunityMappingTags {

    val linpackScala = new ScalaLib(pack = "org.linpack") {
      val invertMatr = ScalaFunc('invertMatrixDouble, ScalaArg(ScalaType('lapack_matr), 'm), ScalaArg(ScalaType('T1), 'V1))()
    }

    val lms = new EmbeddedObject("lms") {
      val arrayLength = ScalaFunc('arrayLength)()
    }

    val mapScalanCE2Scala = {
      import scala.language.reflectiveCalls

      Map[Method, ScalaFunc]()
    }

    val mapping = new ScalaMapping {
      val functionMap = mapScalanCE2Scala
    }
  }

  new CppMappingDSL with CommunityMappingTags {
    import scala.language.reflectiveCalls

    val linpackCpp = new CppLib("linpack.h", "linpack.o") {
      val invertMatr = CppFunc("invertMatrix", CppArg(CppType("lapack_matr"), "m"), CppArg(CppType("T2"), "V2"))
      val transMatr = CppFunc("transMatrixDouble")
    }

    val mapScalanCE2Cpp = {
      import scalanCE._
      Map(
        matrixPack.matrixFam.matrix.invert -> linpackCpp.invertMatr
      )
    }

    val mapping = new CppMapping {
      val functionMap = mapScalanCE2Cpp
    }
  }
}

