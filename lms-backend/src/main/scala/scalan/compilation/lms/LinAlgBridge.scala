package scalan.compilation.lms

import scalan.compilation.lms.common.VectorOpsExp
import scalan.linalgebra.{MatricesDslExp, LinAlgMethodMappingDSL}

trait LinAlgBridge extends CoreBridge with LinAlgMethodMappingDSL {
  override val scalan: MatricesDslExp
  import scalan._

  val lms: CoreLmsBackend with VectorOpsExp

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: DotSparse[_] => "array_dotProductSparse"
    case _ => super.lmsMethodName(d, primitiveName)
  }
}
