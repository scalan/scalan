package scalan.compilation.lms.scalac

import scala.lms.common._
import scalan.compilation.lms.arrays.{ScalaGenArrayMutation, ScalaGenFatArrayLoopsFusionOpt}
import scalan.compilation.lms.common._
import scalan.compilation.lms.{BaseCodegen, LmsBackendFacade}

class ScalaCoreCodegen[BackendCake <: LmsBackendFacade](backend: BackendCake) extends
    BaseCodegen[BackendCake]
    with ScalaGenObjectOpsExt //from scalan.compilation.lms.common
    with ScalaGenArrayOps
    with ScalaGenListOps // todo may be our ScalaGenLstOps should extend lms's ScalaGenListOps?
    with ScalaGenLstOps //from scalan.compilation.lms.common
    with ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenOrderingOps
    with ScalaGenBooleanOps with ScalaGenStructExt with ScalaGenStringOps
    with ScalaGenEitherOps //from scalan.compilation.lms.common
    with ScalaGenTupleOps
    with ScalaGenFatArrayLoopsFusionOpt
    with ScalaGenArrayMutation
    with ScalaGenIfThenElseFat with LoopFusionOpt with ScalaGenCastingOps with ScalaGenMathOps
    with ScalaGenMethodCallOps //from scalan.compilation.lms.common
    with ScalaGenHashMapOps with ScalaGenIterableOps with ScalaGenWhile with ScalaGenIfThenElse
    with ScalaGenVariables with ScalaGenArrayBuilderOps with ScalaGenExceptionOps with ScalaGenTupledFunctions
    with ScalaGenRangeOps
    with ScalaGenMiscOpsExt
    with ScalaGenExtNumOps with ScalaGenSystemOps //from scalan.compilation.lms.common
    with ScalaGenArrayOpsExt {
  val IR: BackendCake = backend

  import IR._

  //def codeExtension: String = "scala"

  override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

  private def isTuple2(name: String) = name.startsWith("Tuple2")

  override def remap[A](m: Manifest[A]) =
    if (m.equals(LmsType.wildCard)) "_"
    else if (isTuple2(m.runtimeClass.getSimpleName)) {
      if (m.typeArguments.length == 2) src"(${m.typeArguments(0)}, ${m.typeArguments(1)})"
      else m.toString
    }
    else super.remap(m)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(ClassTag(name), elems) if isTuple2(name) =>
      emitValDef(sym, "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
