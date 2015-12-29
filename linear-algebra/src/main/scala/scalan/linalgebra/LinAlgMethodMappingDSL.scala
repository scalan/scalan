package scalan.linalgebra

import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.language.ScalaMapping._
import scalan.compilation.language.CxxMapping._

// TODO separate collection mapping
trait LinAlgMethodMappingDSL extends MethodMappingDSL {
  // TODO map Collection.length to LMS method arrayLength
  // TODO map AbstractMatrix.invert

//  trait LinAlgMappingTags extends MappingTags {
//
//    import scala.reflect.runtime.universe.typeOf
//
//    val tyMatrix = typeOf[Matrices#Matrix[_]]
//
//    val collectionsPack = new EPackage("scalan.collections") {
//      val collectionsFam = new EModule('Collections) {
//        val collection = new EType('Collection) {
//          val length = EMethod('length)
//          val arr = EMethod('arr)
//        }
//      }
//    }
//    val matrixPack = new EPackage("scalan.linalgebra") {
//      val matrixFam = new EModule('Matrices) {
//        val matrix = new EType('AbstractMatrix) {
//          val invert = EMethod('invert)
//        }
//      }
//    }
//  }
//
//  new ScalaMappingDSL with LinAlgMappingTags {
//
//    val linpackScala = new ScalaLib(pack = "org.linpack") {
//      val invertMatr = ScalaFunc('invertMatrixDouble, ScalaArg(ScalaType('lapack_matr), 'm), ScalaArg(ScalaType('T1), 'V1))()
//    }
//
//    val lms = new EmbeddedObject("lms") {
//      val arrayLength = ScalaFunc('arrayLength)()
//    }
//
//    val mapScalanCE2Scala = {
//      Map[EMethod, ScalaFunc]()
//    }
//
//    val mapping = new ScalaMapping {
//      val functionMap = mapScalanCE2Scala
//    }
//  }
//
//  new CppMappingDSL with LinAlgMappingTags {
//    import scala.language.reflectiveCalls
//
//    val linpackCpp = new CppLib("linpack.h", "linpack.o") {
//      val invertMatr = CppFunc("invertMatrix", CppArg(CppType("lapack_matr"), "m"), CppArg(CppType("T2"), "V2"))
//      val transMatr = CppFunc("transMatrixDouble")
//    }
//
//    val mapScalanCE2Cpp = {
//      Map(
//        matrixPack.matrixFam.matrix.invert -> linpackCpp.invertMatr
//      )
//    }
//
//    val mapping = new CppMapping {
//      val functionMap = mapScalanCE2Cpp
//    }
//  }


//  trait LinAlgMappingTags extends MappingTags {
//
//    import scala.reflect.runtime.universe.typeOf
//
//    val tyMatrix = typeOf[Matrices#AbstractMatrix[_]]
//
//    val collectionsPack = new EPackage("scalan.collections") {
//      val collectionsFam = new EModule('Collections) {
//        val collection = new EType('Collection) {
//          val length = EMethod('length)
//          val arr = EMethod('arr)
//        }
//      }
//    }
//    val matrixPack = new EPackage("scalan.linalgebra") {
//      val matrixFam = new EModule('Matrices) {
//        val matrix = new EType('AbstractMatrix) {
//          val invert = EMethod('invert)
//        }
//      }
//    }
//  }
//
//  new ScalaMappingDSL with LinAlgMappingTags {
//
//    val linpackScala = new ScalaLib(pack = "org.linpack") {
//      val invertMatr = ScalaFunc('invertMatrixDouble, ScalaArg(ScalaType('lapack_matr), 'm), ScalaArg(ScalaType('T1), 'V1))()
//    }
//
//    val lms = new EmbeddedObject("lms") {
//      val arrayLength = ScalaFunc('arrayLength)()
//    }
//
//    val mapScalanCE2Scala = {
//      Map[EMethod, ScalaFunc]()
//    }
//
//    val mapping = new ScalaMapping {
//      val functionMap = mapScalanCE2Scala
//    }
//  }
//
//  new CppMappingDSL with LinAlgMappingTags {
//    import scala.language.reflectiveCalls
//
//    val linpackCpp = new CppLib("linpack.h", "linpack.o") {
//      val invertMatr = CppFunc("invertMatrix", CppArg(CppType("lapack_matr"), "m"), CppArg(CppType("T2"), "V2"))
//      val transMatr = CppFunc("transMatrixDouble")
//    }
//
//    val mapScalanCE2Cpp = {
//      Map(
//        matrixPack.matrixFam.matrix.invert -> linpackCpp.invertMatr
//      )
//    }
//
//    val mapping = new CppMapping {
//      val functionMap = mapScalanCE2Cpp
//    }
//  }
}
