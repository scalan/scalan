package scala.lms.common

import scala.collection.mutable.ArrayBuilder
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

trait ArrayBuilderOps extends Base {

  object ArrayBuilder {
    def make[A:Manifest] = arraybuilder_make()
  }

  implicit def repToArrayBuilderOps[A:Manifest](l: Rep[ArrayBuilder[A]]) = new ArrayBuilderOpsCls(l)

  class ArrayBuilderOpsCls[A:Manifest](l: Rep[ArrayBuilder[A]]) {
    def +=(e: Rep[A])(implicit pos: SourceContext) = arraybuilder_append(l,e)
    def append(l: Rep[ArrayBuilder[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuilder_append(l,e)
    def clear() = arraybuilder_clear(l)
    def result(implicit pos: SourceContext) = arraybuilder_result(l)
  }

  def infix_+=[A:Manifest](l: Rep[ArrayBuilder[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuilder_append(l, e)

  /* when mixed in with OptiML, one of these infix operations causes an NPE in the scala-virtualized compiler */ //TR: still the case?
  /*
  def infix_+=[A:Manifest](l: Rep[ArrayBuilder[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuilder_append(l, e)
  def infix_append[A:Manifest](l: Rep[ArrayBuilder[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuilder_append(l, e)
  def infix_result[A:Manifest](l: Rep[ArrayBuilder[A]])(implicit pos: SourceContext) = arraybuilder_result(l)
  */

  def arraybuilder_append[A:Manifest](l: Rep[ArrayBuilder[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def arraybuilder_make[A:Manifest]()(implicit pos: SourceContext): Rep[ArrayBuilder[A]]
  def arraybuilder_clear[A:Manifest](l: Rep[ArrayBuilder[A]]): Rep[Unit]
  def arraybuilder_result[A:Manifest](x: Rep[ArrayBuilder[A]])(implicit pos: SourceContext): Rep[Array[A]]
}

trait ArrayBuilderOpsExp extends ArrayBuilderOps with EffectExp {
  case class ArrayBuilderMake[A:Manifest]() extends Def[ArrayBuilder[A]]  {
    val mA = manifest[A]
  }
  case class ArrayBuilderAppend[A:Manifest](l: Exp[ArrayBuilder[A]], e: Exp[A]) extends Def[Unit]
  case class ArrayBuilderClear[A:Manifest](l: Exp[ArrayBuilder[A]]) extends Def[Unit]
  case class ArrayBuilderResult[A:Manifest](x: Exp[ArrayBuilder[A]]) extends Def[Array[A]]

  def arraybuilder_make[A:Manifest]()(implicit pos: SourceContext) = reflectMutable(ArrayBuilderMake())
  def arraybuilder_append[A:Manifest](l: Exp[ArrayBuilder[A]], e: Exp[A])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBuilderAppend(l, e))
  def arraybuilder_clear[A:Manifest](l: Exp[ArrayBuilder[A]]) = reflectWrite(l)(ArrayBuilderClear(l))
  def arraybuilder_result[A:Manifest](x: Exp[ArrayBuilder[A]])(implicit pos: SourceContext) = ArrayBuilderResult(x)

  //////////////
  // mirroring

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayBuilderResult(l) => ArrayBuilderResult(f(l))
    case ArrayBuilderAppend(l,r) => ArrayBuilderAppend(f(l),f(r))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??
}

trait BaseGenArrayBuilderOps extends GenericCodegen {
  val IR: ArrayBuilderOpsExp
  import IR._
}

trait ScalaGenArrayBuilderOps extends BaseGenArrayBuilderOps with ScalaGenEffect {
  val IR: ArrayBuilderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayBuilderMake() => emitValDef(sym, src"scala.collection.mutable.ArrayBuilder.make[${a.mA}]")
    case ArrayBuilderAppend(l, e) => emitValDef(sym, src"$l += $e")
    case ArrayBuilderClear(l) => emitValDef(sym, src"$l.clear()")
    case ArrayBuilderResult(x) => emitValDef(sym, src"$x.result")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayBuilderOps extends BaseGenArrayBuilderOps with CLikeGenBase {
  val IR: ArrayBuilderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenArrayBuilderOps extends CudaGenEffect with CLikeGenArrayBuilderOps
trait OpenCLGenArrayBuilderOps extends OpenCLGenEffect with CLikeGenArrayBuilderOps
trait CGenArrayBuilderOps extends CGenEffect with CLikeGenArrayBuilderOps
