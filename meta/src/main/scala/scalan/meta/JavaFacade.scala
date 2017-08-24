package scalan.meta

import scalan.meta.ScalanAst.{SValDef, STpeExpr, SClassDef, STpePrimitive, SBodyItem}
import PrintExtensions._

class CodeEmitter[T](emit: T => String) extends (T => String) {
  override def apply(x: T) = emit(x)
}

object KotlinEmitters {
  def emitClassDef(x: SClassDef): String = {
    val bodyItems = for (item <- x.body) yield {
      emit(item)
    }
    s"""
      |class ${x.name}() {
      |  ${bodyItems.rep()}
      |}
      """.stripMargin
  }
  def emitBodyItem(x: SBodyItem): String = x match {
    case SValDef(n, tpe, isLazy, _, expr) =>
      s"""val $n${tpe.opt(t => s": ${emit(t)}")} = TODO("")"""
  }
  def emitTpeExpr(x: STpeExpr): String = x match {
    case STpePrimitive(n, _) => n
    case _ => ???
  }
  implicit val classDefEmitter = new CodeEmitter[SClassDef](emitClassDef)
  implicit val bodyItemEmitter = new CodeEmitter[SBodyItem](emitBodyItem)
  implicit val tpeExprEmitter  = new CodeEmitter[STpeExpr](emitTpeExpr)
  def emit[T](x: T)(implicit e: CodeEmitter[T]): String = e(x)
}

object JavaFacade {
  def nil[T](): List[T] = Nil
  def list[T](xs: T*): List[T] = List(xs: _*)
  def none[T](): Option[T] = None
  def some[T](x: T): Option[T] = Some(x)
  def emitCode[T](x: T)(implicit emitter: CodeEmitter[T]): String = emitter(x)
}
