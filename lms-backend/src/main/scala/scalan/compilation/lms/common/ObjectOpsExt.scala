package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait ObjectOpsExt extends Base {

  def newObj[A: Manifest](className: String, args: Seq[Rep[_]]): Rep[A]
}

trait ObjectOpsExtExp extends ObjectOpsExt with BaseExp {

  case class NewObj[A: Manifest](className: String, args: Seq[Rep[_]]) extends Def[A] {
    val m = manifest[A]
  }

  def newObj[A: Manifest](className: String, args: Seq[Rep[_]]): Exp[A] = {
    NewObj[A](className, args)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case NewObj(className, args) => newObj(className, args.map(arg => f(arg)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }
}

trait ScalaGenObjectOpsExt extends ScalaGenBase {
  val IR: ObjectOpsExtExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewObj(className, args) => emitValDef(sym, src"new $className(${(args map quote).mkString(",")})")
    case _ => super.emitNode(sym, rhs)
  }
}
