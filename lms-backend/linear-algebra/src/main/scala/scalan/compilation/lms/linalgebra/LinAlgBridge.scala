package scalan.compilation.lms.linalgebra

import scalan.compilation.lms.collections.CollectionsBridgeScala
import scalan.compilation.lms.{ScalaCoreLmsBackend, CoreBridge, CoreLmsBackend}
import scalan.linalgebra.{VectorsDslExp, LinAlgMethodMappingDSL}

trait LinAlgBridge extends CoreBridge with LinAlgMethodMappingDSL {
  override val scalan: VectorsDslExp
  import scalan._

  val lms: CoreLmsBackend with VectorOpsExp

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: DotSparse[_] => "array_dotProductSparse"
    case _ => super.lmsMethodName(d, primitiveName)
  }
}

trait LinAlgBridgeScala extends CollectionsBridgeScala with LinAlgBridge {
  val lms: ScalaCoreLmsBackend with VectorOpsExp
}
