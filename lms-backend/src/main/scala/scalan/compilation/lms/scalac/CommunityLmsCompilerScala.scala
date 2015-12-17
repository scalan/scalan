package scalan.compilation.lms.scalac

import scalan.compilation.lms.{ScalaLinAlgLmsBackend, CommunityBridgeScala}
import scalan.linalgebra.MatricesDslExp

class CommunityLmsCompilerScala[+ScalanCake <: MatricesDslExp](_scalan: ScalanCake) extends LmsCompilerScala(_scalan) with CommunityBridgeScala {
  override val lms = new ScalaLinAlgLmsBackend
}