package scalan.compilation.lms

import scala.virtualization.lms.common._
import scala.virtualization.lms.epfl.test7.ScalaGenFatArrayLoopsFusionOpt
import scalan.compilation.lms.common.{ScalaGenVectorOps, ScalaGenEitherOps}

class CoreScalaLmsBackend extends CoreLmsBackend { self =>

  trait Codegen extends ScalaGenEffect with ScalaGenStruct with ScalaGenArrayOps with ScalaGenListOps with ScalaGenNumericOps
  with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenEitherOps with ScalaGenOrderingOps with ScalaGenBooleanOps
  with ScalaGenTupleOps with ScalaGenFatArrayLoopsFusionOpt with ScalaGenIfThenElseFat with LoopFusionOpt
  with ScalaGenCastingOps {

    val IR: self.type = self
    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

    private def isTuple(name: String) =
      name.startsWith("Tuple2") || name.startsWith("Tuple3") || name.startsWith("Tuple4") || name.startsWith("Tuple5")

    override def remap[A](m: Manifest[A]) =
      if (isTuple(m.runtimeClass.getSimpleName)) m.toString
      else super.remap(m)

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case Struct(ClassTag(name), elems) if isTuple(name) =>
        emitValDef(sym, "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }

  val codegen = new Codegen {}
}

class CommunityScalaLmsBackend extends CoreScalaLmsBackend with CommunityLmsBackend { self  =>

  override val codegen = new Codegen with ScalaGenVectorOps {
    override val IR: self.type = self
  }
}
