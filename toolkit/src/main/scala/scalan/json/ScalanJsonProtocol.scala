package scalan.json

import spray.json.{deserializationError, DefaultJsonProtocol, PrettyPrinter, JsObject, JsArray, JsonFormat, JsString, JsValue, JsBoolean}
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.immutable.ListMap
import scalan.{Scalan, Lazy}
import scala.collection.mutable.{Map => MMap}
import scalan.meta.PrintExtensions._

/** Extension of spray.json's default protocol to support Scalan specific types.
  * See https://github.com/spray/spray-json  for details.
  * Each instance of protocol is parameterized by Scalan cake containing IR nodes
  * (see Def[_] hierarchy of classes).
  * */
class ScalanJsonProtocol[C <: Scalan](val ctx: C) extends DefaultJsonProtocol with ScalanJsonContext[C] {

  import ctx._
//  {PGraph, Lambda, Def, Elem, parsers, ElemOps, TypeDesc, emptySubst, Const, MethodCall, TypeDescOps,
//              Sym, syms, ApplyBinOp, ApplyUnOp, TableEntry, MapTransformer, fun, toRep}
  import parsers.{genTypeExpr, global, parseType}

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

  implicit object SymFormat extends JsonFormat[Sym] {
    private def opName(d: Def[_]) = d match {
      case d: ApplyUnOp[_, _] => d.op.opName
      case d: ApplyBinOp[_, _] => d.op.opName
      case d: MethodCall => d.getClass.getSimpleName
      case First(_) | Second(_) => d.getClass.getSimpleName
      case _ => ctx.!!!(s"Don't know how to get opName for $d")
    }
    private def addDef(opName: String, args: Seq[Sym], eRes: Element): Sym = (opName, args, eRes) match {
      case NumericBinOp(d) => d
      case DeclaredUnOp(d) => d
      case ("First", Seq(IsPair(p: RPair[a,b])), _) => reifyObject(First(p))
      case ("Second", Seq(IsPair(p: RPair[a,b])), _) => reifyObject(Second(p))
      case _ =>
        ctx.!!!(s"Cannot add definition for operation $opName($args): $eRes.")
    }

    def write(sym: Sym) = sym match {
      case Def(Const(c)) =>
        JsArray(JsString("Const"), JsString(c.toString), elementFormat.write(sym.elem))
      case Def(MethodCall(obj, m, args, neverInvoke)) =>
        val params = "MethodCall" :: mapSym(obj) :: m.getDeclaringClass.getName :: m.getName :: neverInvoke.toString ::
              args.map(mapMCallArg(_)(mapSym(_)))
//            args.map { _ match {
//              case s: Sym => mapSym(s)
//              case x => ctx.!!!(s"MethodCall with non-Sym argument $x is not supported for Json serialization of $d")
//            }}
        JsArray(params.map(JsString(_)) :+ elementFormat.write(sym.elem): _*)
      case Def(d) =>
        val args = syms(d).map(mapSym(_))
        val str = s"${opName(d)}(${args.rep()})"
        JsArray(JsString(str), elementFormat.write(d.selfType))
    }

    def readDefs(defs: Seq[(Int, JsValue)]): Unit =
      for ((id, jsDef) <- defs) {
        val s = SymFormat.read(jsDef)
        idToSym += (id -> s)
      }

    def read(json: JsValue): Sym = json match {
      case JsLambda(varId, eVar, lamBody, roots) =>
        val f = fun({ x: Sym =>
          idToSym += (varId -> x)
          readDefs(sortedSchedule(lamBody))
          mapId(roots(0))
        })(Lazy(eVar))
        f
      case JsMethodCall(obj, className, methodName, neverInvoke, eRes, args) =>
        val m = getMethod(className, methodName, args.map(_.getClass))
        val s = mkMethodCall(obj, m, args.toList, neverInvoke, eRes)
        s
      case JsArray(Vector(JsString("Const"), jsValue, jsElem)) =>
        val e = elementFormat[Any].read(jsElem)
        val value = fromJsonValue(jsValue, e)
        val s = toRep(value)(e)
        s
      case JsDef(opName, argIds, eRes) =>
        val argSyms = argIds.map(idToSym(_))
        val s = addDef(opName, argSyms, eRes)
        s
      case _ => deserializationError(s"Cannot read Def from $json")
    }
  }

  implicit object LambdaFormat extends JsonFormat[Lambda[_, _]] {
    def write(lam: Lambda[_, _]) = {
      val fields = lam.schedule.map {
        case TableEntry(s, Lambda(l, _, x, y)) =>
          (mapSym(s), LambdaFormat.write(l))
        case TableEntry(s, d) =>
          (mapSym(s), SymFormat.write(s))
      }
      JsObject(ListMap(Seq( // ListMap to preserve order
        ("type", JsString("Lambda")),
        ("var", JsArray(JsString(mapSym(lam.x)), elementFormat.write(lam.x.elem)))) ++
          fields ++
          Seq(("roots", JsString(mapSym(lam.y)))): _*))
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
          (mapSym(s), SymFormat.write(s))
      }
      val roots = g.roots.map(mapSym(_))
      JsObject(ListMap( // ListMap to preserve order
        Seq(("type", JsString("ProgramGraph")), ("roots", JsString(roots.rep()))) ++
            fields: _*))
    }

    def read(json: JsValue) = json match {
      case JsProgramGraph(fields, rootIds) =>
        SymFormat.readDefs(sortedSchedule(fields))
        val rootSyms = rootIds.map(mapId(_))
        new PGraph(rootSyms.toList)(MapTransformer.ops)
      case _ => deserializationError("String expected of type term")
    }
  }

}

