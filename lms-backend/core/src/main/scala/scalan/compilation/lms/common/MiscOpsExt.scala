package scalan.compilation.lms.common

import scala.lms.common.{ScalaGenMiscOps, MiscOpsExp, MiscOps}
import scala.reflect.SourceContext

trait MiscOpsExt extends MiscOps {
  def readline(implicit pos: SourceContext): Rep[String]
}

trait MiscOpsExtExp extends MiscOpsExt with MiscOpsExp {
  case class Readline() extends Def[String]

  def readline(implicit pos: SourceContext) = reflectEffect(Readline()) // TODO: simple effect

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(Readline(), u, es) => reflectMirrored(Reflect(Readline(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenMiscOpsExt extends ScalaGenMiscOps {
  val IR: MiscOpsExtExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Readline() => emitValDef(sym, src"Predef.readLine()")
    case _ => super.emitNode(sym, rhs)
  }
}
