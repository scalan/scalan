package scalan.compilation.lms.uni

import scalan.compilation.lms.common.JNILmsOpsExp
import scalan.compilation.lms.cxx.sharedptr.CxxCodegen
import scalan.compilation.lms.{BaseCodegen, LmsBackendFacade, JNIBridge}
import scalan.{JNIExtractorOpsExp, ScalanCtxExp}

/**
 * Created by adel on 6/8/15.
 */
object KnownCodegens  extends Enumeration {
  type Codegens = Value
  val Scala, Cxx = Value //maybe Cuda

  //alternative is add Element for this type
  def fromString(x:String): Codegens = x match {
    case "Scala" => Scala
    case "Cxx" => Cxx
  }
  def toString(x:Codegens):String = x match {
    case Scala => "Scala"
    case Cxx => "Cxx"
  }

  def pairFromString(x:String): (Codegens, Codegens) = x match {
    case "Scala2Cxx" => (Scala, Cxx)
    case _ => throw new UnsupportedOperationException(s"Codegens: unknown converter '$x'")
  }
  def pairToString(x:Codegens, y: Codegens): String = (x, y) match {
    case (Scala, Cxx) =>  "Scala2Cxx"
    case _ => throw new UnsupportedOperationException(s"Codegens: unknown converter from '$x' to '$y'")
  }


  def getAdapterByString[ScalanCake <: ScalanCtxExp with JNIExtractorOpsExp with JNIBridge]
                (sc: ScalanCake, xy: String): AdapterBase[ScalanCake] =
    getAdapter(sc, pairFromString(xy))

  //[ScalanCake <: ScalanCtxExp, BackendCake <: LmsBackendFacade]
  def getAdapter[ScalanCake <: ScalanCtxExp with JNIExtractorOpsExp with JNIBridge]
                (sc: ScalanCake, xy: (Codegens, Codegens)): AdapterBase[ScalanCake] = {
    xy match {
      case (Scala, Cxx) =>  new AdapterScala2Cxx(sc)
      case _ => throw new UnsupportedOperationException(s"Codegens: adapter for pair '$xy' not implemended yet")
    }
  }

  //def getCodegenByString//???[ScalanCake <: ScalanCtxExp with JNIExtractorOpsExp with JNIBridge, BackendCake <: LmsBackendFacade with JNILmsOpsExp]

  //[ScalanCake <: ScalanCtxExp, BackendCake <: LmsBackendFacade]
  def getCodegen[BackendCake <: LmsBackendFacade with JNILmsOpsExp]
                  (bc: BackendCake, codegen: Codegens): BaseCodegen[BackendCake] = {
    codegen match {
      case Cxx   =>  new CxxCodegen(bc)
      case Scala =>  new JniCallCodegen(bc, null, "")
      case _ => throw new UnsupportedOperationException(s"Codegens: codegen '$codegen' not implemended yet")
    }
  }

}

