package scalan.json

import spray.json.{deserializationError, DefaultJsonProtocol, PrettyPrinter, JsObject, JsArray, JsonFormat, JsString, JsValue}

import scala.collection.immutable.ListMap
import scalan.Scalan
import scala.collection.mutable.{Map => MMap}
import scalan.meta.PrintExtensions._

class ScalanJsonProtocol[C <: Scalan](val ctx: C) extends DefaultJsonProtocol {
  import ctx._
  import parsers._

  implicit val context = parsers.context // for resolution and disambiguation
  implicit val parseCtx = new ParseCtx(false)
  implicit val genCtx = new GenCtx(context, toRep = false)
  val symToId = MMap.empty[Rep[_], Int]
  private var currId: Int = 0

  def newId(): Int = {currId += 1; currId }

  def mapSym(s: Rep[_]): String = symToId.get(s) match {
    case Some(id) => s"s$id"
    case None =>
      val id = newId()
      symToId += (s -> id)
      s"s$id"
  }

  implicit def elementFormat[T]: JsonFormat[Elem[T]] = new JsonFormat[Elem[T]] {
    def write(e: Elem[T]) = {
      val tpe = e.toTpeExpr
      val ty = genTypeExpr(tpe)
      val tpeStr = global.showCode(ty)
      JsString(tpeStr)
    }

    def read(json: JsValue) = json match {
      case JsString(tpeStr) =>
        val ty = parseType(tpeStr)
        val elem = TypeDesc(ty, emptySubst).asElem[T]
        elem
      case _ => deserializationError("String expected of type term")
    }
  }

  implicit def defFormat[T]: JsonFormat[Def[T]] = new JsonFormat[Def[T]] {
    def opName(d: Def[T]) = d match {
      case d: ApplyUnOp[_,_] => d.op.opName
      case d: ApplyBinOp[_,_] => d.op.opName
      case _ => d.getClass.getSimpleName
    }
    def write(d: Def[T]) = d match {
      case Const(c) =>
        JsArray(JsString("Const"), JsString(c.toString), elementFormat.write(d.selfType))
      case d =>
        val args = syms(d).map(mapSym(_))
        val str = s"${opName(d)}(${args.rep()})"
        JsArray(JsString(str), elementFormat.write(d.selfType))
    }

    def read(json: JsValue) = json match {
      case JsString(tpeStr) =>
        ctx.???
      case _ => deserializationError("String expected of type term")
    }
  }

  implicit object LambdaFormat extends JsonFormat[Lambda[_,_]] {
    def write(lam: Lambda[_,_]) = {
      val fields = lam.schedule.map {
        case TableEntry(s, Lambda(l, _, x, y)) =>
          (mapSym(s), LambdaFormat.write(l))
        case TableEntry(s, d) =>
          (mapSym(s), defFormat.write(d))
      }
      JsObject(ListMap(Seq(    // ListMap to preserve order
        ("type", JsString("Lambda")),
        ("var", JsArray(JsString(mapSym(lam.x)), elementFormat.write(lam.x.elem)))) ++
        fields: _*))
    }

    def read(json: JsValue) = json match {
      case JsObject(tpeStr) => ctx.???
      case _ => deserializationError("JsObject of Lambda definition expected")
    }
  }

  implicit object ProgramGraphFormat extends JsonFormat[PGraph] {
    def write(g: PGraph) = {
      val fields = g.schedule.map {
        case TableEntry(s, Lambda(l, _, x, y)) =>
          (mapSym(s), LambdaFormat.write(l))
        case TableEntry(s, d) =>
          (mapSym(s), defFormat.write(d))
      }
      JsObject(ListMap(Seq(    // ListMap to preserve order
        ("type", JsString("ProgramGraph"))) ++
        fields: _*))
    }

    def read(json: JsValue) = json match {
      case JsString(tpeStr) => ctx.???
      case _ => deserializationError("String expected of type term")
    }
  }

}

