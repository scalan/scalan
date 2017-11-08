package scalan.json

import spray.json.{deserializationError, DefaultJsonProtocol, PrettyPrinter, JsObject, JsArray, JsonFormat, JsString, JsValue}
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
class ScalanJsonProtocol[C <: Scalan](val ctx: C) extends DefaultJsonProtocol {
  import ctx._
  import parsers._

  implicit val context = parsers.context // for resolution and disambiguation
  implicit val parseCtx = new ParseCtx(false)
  implicit val genCtx = new GenCtx(context, toRep = false)
  val symToId = MMap.empty[Sym, Int]
  val idToSym = MMap.empty[Int, Sym]
  private var currId: Int = 0

  def newId(): Int = {currId += 1; currId }

  def mapSym(s: Sym): String = symToId.get(s) match {
    case Some(id) => s"s$id"
    case None =>
      val id = newId()
      symToId += (s -> id)
      s"s$id"
  }

  def mapId(id: Int): Sym = idToSym(id)

  def readId(sId: String) = sId.trim.stripPrefix("s").toInt

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
      case d: ApplyUnOp[_, _] => d.op.opName
      case d: ApplyBinOp[_, _] => d.op.opName
      case _ => d.getClass.getSimpleName
    }

    def write(d: Def[T]) = d match {
      case Const(c) =>
        JsArray(JsString("Const"), JsString(c.toString), elementFormat.write(d.selfType))
      case d =>
        val args = syms(d).map(mapSym(_))
        val str = s"${opName(d) }(${args.rep() })"
        JsArray(JsString(str), elementFormat.write(d.selfType))
    }

    def read(json: JsValue) = json match {
      case JsString(tpeStr) =>
        ctx.???
      case _ => deserializationError("String expected of type term")
    }
  }

  implicit object LambdaFormat extends JsonFormat[Lambda[_, _]] {
    def write(lam: Lambda[_, _]) = {
      val fields = lam.schedule.map {
        case TableEntry(s, Lambda(l, _, x, y)) =>
          (mapSym(s), LambdaFormat.write(l))
        case TableEntry(s, d) =>
          (mapSym(s), defFormat.write(d))
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

  object JsDef {
    val fmt = """([\w$+\-*\/]+)\(([s\d\s,]+)\)""".r

    def unapply(json: JsValue): Option[(String, Seq[Int], Elem[_])] = json match {
      case JsArray(Vector(JsString(fmt(opName, sArgs)), jsElem)) =>
        val argIds = sArgs.split(',').map(readId(_)).toSeq
        val eRes = elementFormat.read(jsElem)
        Some((opName, argIds, eRes))
      case _ => None
    }
  }

  object JsLambda {
    val specialFields = Seq("type", "var", "roots")

    def unapply(jsLam: JsValue): Option[(Int, Elem[Any], Map[String, JsValue], Seq[Int])] = jsLam match {
      case obj @ JsObject(fields) =>
        obj.getFields(specialFields: _*) match {
          case Seq(JsString("Lambda"), JsArray(Vector(JsString(sId), jsType)), JsString(roots)) =>
            val varId = readId(sId)
            val eVar = elementFormat.read(jsType)
            val defs = fields -- specialFields
            val res = roots.split(',').map(readId(_)).toSeq
            Some((varId, eVar.asElem[Any], defs, res))
          case _ => None
        }
      case _ => None
    }
  }

  object JsProgramGraph {
    def unapply(json: JsValue): Option[(Map[String, JsValue], Seq[Int])] = json match {
      case JsObject(fields) =>
        (fields.get("type"), fields.get("roots")) match {
          case (Some(JsString("ProgramGraph")), Some(JsString(sRoots))) =>
            val roots = sRoots.split(',').map(readId(_)).toSeq
            Some((fields -- Seq("type", "roots"), roots))
          case _ => None
        }
      case _ => None
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
      val roots = g.roots.map(mapSym(_))
      JsObject(ListMap( // ListMap to preserve order
        Seq(("type", JsString("ProgramGraph")), ("roots", JsString(roots.rep()))) ++
            fields: _*))
    }

    def read(json: JsValue) = json match {
      case JsProgramGraph(fields, rootIds) =>
        readDefs(fields.toList.sortBy(_._1))
        val rootSyms = rootIds.map(mapId(_))
        new PGraph(rootSyms.toList)(MapTransformer.ops)
      case _ => deserializationError("String expected of type term")
    }

    private def readDefs(defs: List[(String, JsValue)]): Unit =
      for ((sId, jsDef) <- defs) {
        val id = readId(sId)
        jsDef match {
          case JsLambda(varId, eVar, lamBody, roots) =>
            val f = fun({ x: Sym =>
              idToSym += (varId -> x)
              readDefs(lamBody.toList.sortBy(_._1))
              mapId(roots(0))
            })(Lazy(eVar))
            idToSym += (id -> f)
          case JsArray(Vector(JsString("Const"), JsString(sValue), jsElem)) =>
            val e = elementFormat[Any].read(jsElem)
            val value = readConstValue(sValue, e)
            val s = toRep(value)(e)
            idToSym += (id -> s)
          case JsDef(opName, argIds, eRes) =>
            val argSyms = argIds.map(idToSym(_))
            val s = addDef(opName, argSyms, eRes)
            idToSym += (id -> s)
        }
      }

    def readConstValue[A](sValue: String, e: Elem[A]): A = (e match {
      case _ if e == IntElement => sValue.toInt
      case _ =>
        ctx.!!!(s"""Don't know how to parse "$sValue" into type of $e.""")
    }).asInstanceOf[A]

    private def addDef(opName: String, args: Seq[Sym], eRes: Element): Sym = (opName, args, eRes) match {
      case NumericBinOp(d) => d
      case DeclaredUnOp(d) => d
      case ("First", Seq(IsPair(p: RPair[a,b])), _) => reifyObject(First(p))
      case ("Second", Seq(IsPair(p: RPair[a,b])), _) => reifyObject(Second(p))
      case _ =>
        ctx.!!!(s"Cannot add definition for operation $opName($args): $eRes.")
    }

    type Element = Elem[_]

    object DeclaredUnOp {
      val unOpClass = classOf[UnOp[_,_]]
      val declaredOps = {
        val ops = for {
          int <- ctx.getClass.getInterfaces
          m <- int.getDeclaredMethods if unOpClass.isAssignableFrom(m.getReturnType)
        } yield {
          val op = m.invoke(ctx).asInstanceOf[UnOp[Any,Any]]
          (op.opName, op)
        }
        ops.toMap
      }
      def unapply(in: (String, Seq[Sym], Element)): Option[ApplyUnOp[_, _]] = declaredOps.get(in._1) match {
        case Some(op: UnOp[a,b]) =>
          Some(ApplyUnOp[a,b](op, in._2(0).asRep[a]))
        case _ => None
      }
    }

    object NumericBinOp {
      def unapply(in : (String, Seq[Sym], Element)): Option[ApplyBinOp[_,_]] = {
        val (opName: String, args: Seq[Sym], eRes) = in
        BinOps.get(opName) match {
          case Some(elems) => elems.get(eRes) match {
            case Some(op: BinOp[a,b]) =>
              Some(ApplyBinOp[a,b](op, args(0).asRep[a], args(1).asRep[a]))
            case _ => None
          }
          case None => None
        }
      }

      val BinOps: Map[String, Map[Element, BinOp[_,_]]] = Map(
        "+" -> Map[Element, BinOp[_,_]](
          IntElement -> NumericPlus[Int](numeric[Int])
        ),
        "-" -> Map(
          IntElement -> NumericMinus[Int](numeric[Int])
        ),
        "*" -> Map(
          IntElement -> NumericTimes[Int](numeric[Int])
        ))
    }

  }

}

