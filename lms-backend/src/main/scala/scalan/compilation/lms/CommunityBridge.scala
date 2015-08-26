package scalan.compilation.lms

import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp}

trait CommunityBridge extends CoreBridge with CommunityMethodMappingDSL {
  override val scalan: ScalanCommunityDslExp
  import scalan._

  val lms: CommunityLmsBackendBase

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: DotSparse[_] => "array_dotProductSparse"
    case _ => super.lmsMethodName(d, primitiveName)
  }
}
