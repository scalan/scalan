package scalan.compilation.lms.uni

import scalan.compilation.lms.common.JNILmsOpsExp
import scalan.compilation.lms.cxx.sharedptr.CxxCoreCodegen
import scalan.compilation.lms.{BaseCodegen, LmsBackendFacade}
import scalan.{JNIExtractorOpsExp, ScalanDslExp}

/**
 * Created by adel on 6/8/15.
 */
object KnownCodegens {
  sealed abstract class CodegenType(val id: Int, val name:String)

  case object Scala extends CodegenType(1, "Scala")
  case object Cxx extends CodegenType(2, "Cxx")

  //import EnumerationMacros._ - not supported in sthis ersion of Scala
  //val codegens: Set[CodegenType] = sealedInstancesOf[CodegenType]
  val codegens: Set[CodegenType] = Set(Cxx, Scala)

  def fromString(x:String): CodegenType = codegens.filter(_.name == x).head

  def toString(x:CodegenType):String = x.name

  def pairFromString(x:String): (CodegenType, CodegenType) = x match {
    case "Scala2Cxx" => (Scala, Cxx)
    case _ => throw new UnsupportedOperationException(s"Codegens: unknown converter '$x'")
  }
  def pairToString(x:CodegenType, y: CodegenType): String = (x, y) match {
    case (Scala, Cxx) =>  "Scala2Cxx"
    case _ => throw new UnsupportedOperationException(s"Codegens: unknown converter from '$x' to '$y'")
  }


  def getAdapterByString[ScalanCake <: ScalanDslExp with JNIExtractorOpsExp]
                (sc: ScalanCake, xy: String): AdapterBase[ScalanCake] =
    getAdapter(sc, pairFromString(xy))

  //[ScalanCake <: ScalanDslExp, BackendCake <: LmsBackendFacade]
  def getAdapter[ScalanCake <: ScalanDslExp with JNIExtractorOpsExp]
                (sc: ScalanCake, xy: (CodegenType, CodegenType)): AdapterBase[ScalanCake] = {
    xy match {
      case (Scala, Cxx) =>  new AdapterScala2Cxx(sc)
      case _ => throw new UnsupportedOperationException(s"Codegens: adapter for pair '$xy' not implemended yet")
    }
  }

}
