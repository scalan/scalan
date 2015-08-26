package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait ObjectOpsExt extends Base {

  def newObj[A: Manifest](className: String, args: Seq[Any], newKeyWord: Boolean): Rep[A]
}

trait ObjectOpsExtExp extends ObjectOpsExt with BaseExp {

  case class NewObj[A: Manifest](className: String, args: Seq[Any], newKeyWord: Boolean) extends Def[A] {
    val m = manifest[A]
  }

  def newObj[A: Manifest](className: String, args: Seq[Any], newKeyWord: Boolean): Exp[A] = {
    NewObj[A](className, args, newKeyWord)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case NewObj(className, args, newKeyWord) =>
      val newArgs = args.map {
        case arg: Exp[_] => f(arg)
        case arg => arg
      }
      newObj(className, newArgs, newKeyWord)(mtype(manifest[A])) // TODO check: Why not use the manifest from the NewObj
    case _ => super.mirror(e, f)
  }
}

trait ScalaGenObjectOpsExt extends ScalaGenBase {
  val IR: ObjectOpsExtExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewObj(className, args, newKeyWord) =>
      val newStr = newKeyWord match {
        case true => "new "
        case false => ""
      }
      val argsStr = args.map {
        case e: Exp[_] => quote(e)
        // TODO handle standard implicit arguments (Numeric/Ordering/Manifest/etc.)
        case arg => arg
      }.mkString(",")
      emitValDef(sym, src"$newStr$className($argsStr)")
    case _ => super.emitNode(sym, rhs)
  }
}
