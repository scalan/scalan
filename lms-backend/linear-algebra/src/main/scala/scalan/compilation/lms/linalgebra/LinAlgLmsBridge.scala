package scalan.compilation.lms.linalgebra

import scalan.compilation.lms.collections.CollectionsLmsBridgeScala
import scalan.compilation.lms.{CoreLmsBridge, CoreLmsBackend}
import scalan.linalgebra.LADslExp

trait LinAlgLmsBridge extends CoreLmsBridge {
  override val scalan: LADslExp
  import scalan._

  val lms: CoreLmsBackend with VectorOpsExp

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: DotSparse[_] => "array_dotProductSparse"
    case _ => super.lmsMethodName(d, primitiveName)
  }
}

trait LinAlgLmsBridgeScala extends CollectionsLmsBridgeScala with LinAlgLmsBridge
