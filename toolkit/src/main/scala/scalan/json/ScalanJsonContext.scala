package scalan.json

import java.lang.reflect.Method
import spray.json.{JsObject, JsArray, JsString, JsValue, JsBoolean}
import scala.collection.Seq
import scalan.Scalan
import scala.collection.mutable.{Map => MMap}

trait ScalanJsonContext[C <: Scalan] { self: ScalanJsonProtocol[C] =>
  val ctx: C
  import ctx._
  import parsers._

  implicit val context = parsers.context // for resolution and disambiguation
  implicit val parseCtx = new ParseCtx(false)
  implicit val genCtx = new GenCtx(context, toRep = false, isVirtualized = false)
  val symToId = MMap.empty[Sym, Int]
  val idToSym = MMap.empty[Int, Sym]
  private var currId: Int = 0
  def reset() = {
    currId = 0
    symToId.clear()
    idToSym.clear()
  }
  def newId(): Int = {currId += 1; currId }

  def mapSym(s: Sym): String = symToId.get(s) match {
    case Some(id) => s"s$id"
    case None =>
      val id = newId()
      symToId += (s -> id)
      s"s$id"
  }

  def mapId(id: Int): Sym = idToSym(id)

  def readId(sId: String) = {
    val JsSym.SymFmt(id) = sId
    id.toInt
  }

  def sortedSchedule(body: Map[String, JsValue]): Seq[(Int, JsValue)] =
    body.toSeq.map(p => (readId(p._1), p._2)).sortBy(_._1)

  object JsSym {
    val SymFmt = """^\s*s([0-9]+)\s*$""".r

    def unapply(jsId: JsString) = Some(mapId(readId(jsId.value)))
  }

  object JsDef {
    val DefFmt = """([\w$+\-*\/]+)\(([s\d\s,]+)\)""".r

    def unapply(json: JsValue): Option[(String, Seq[Int], Elem[_])] = json match {
      case JsArray(Vector(JsString(DefFmt(opName, sArgs)), jsElem)) =>
        val argIds = sArgs.split(',').map(readId(_)).toSeq
        val eRes = elementFormat.read(jsElem)
        Some((opName, argIds, eRes))
      case _ => None
    }
  }

  def getMethod(className: String, methodName: String, argTypes: Seq[Class[_]]): Method = {
    val loader = ctx.getClass.getClassLoader
    val clazz = Class.forName(className, true, loader)
    val m = clazz.getMethod(methodName, argTypes: _*)
    m
  }

  def mapMCallArg[A](arg: AnyRef)(f: Sym => A): A = arg match {
    case s: Sym => f(s)
    case x => ctx.!!!(s"MethodCall with non-Sym argument $arg is not supported for Json serialization")
  }

  object JsMethodCall {
    def unapply(json: JsValue): Option[(Sym, String, String, Boolean, Elem[_], Seq[Sym])] = json match {
      case JsArray(Vector(JsString("MethodCall"),
      JsSym(obj), JsString(className), JsString(methodName), JsString(sInvoke), jsResType, args @ _*)) =>
        val argIds = args.map {
          case JsString(sArg) => readId(sArg)
          case js => sys.error(s"Unexpected JsValue $js while parsing $json")
        }
        val argSyms = argIds.map(idToSym(_))
        val eRes = elementFormat.read(jsResType)
        Some((obj, className, methodName, sInvoke.toBoolean, eRes, argSyms))
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

  type Element = Elem[_]

  def toJsonValue[A](v: A, eA: Elem[A]): JsValue = (v, eA: TypeDesc) match {
    case (v: Boolean, BooleanElement) => JsBoolean(v)
    case (_, IntElement) => JsString(v.toString)
    case (_, ShortElement) => JsString(v.toString)
    case (_, LongElement) => JsString(v.toString)
    case (_, ByteElement) => JsString(v.toString)
    case (_, FloatElement) => JsString(v.toString)
    case (_, DoubleElement) => JsString(v.toString)
    case (_, UnitElement) => JsString("()")
    case (_, CharElement) => JsString(v.toString)
    case (_, StringElement) => JsString(v.toString)
    case (_, pe: PairElem[a, b]) =>
      val pv = v.asInstanceOf[(a,b)]
      JsArray(toJsonValue(pv._1, pe.eFst), toJsonValue(pv._2, pe.eSnd))
    case (Left(v), se: SumElem[a, b]) =>
      val lv = v.asInstanceOf[a]
      JsArray(JsString("left"), toJsonValue(lv, se.eLeft))
    case (Right(v), se: SumElem[a, b]) =>
      val rv = v.asInstanceOf[b]
      JsArray(JsString("right"), toJsonValue(rv, se.eRight))
    case _ =>
      ctx.!!!(s"""Don't know how to write "$v" of type $eA to json.""")
  }

  def fromJsonValue[A](jsValue: JsValue, e: Elem[A]): A = ((jsValue, e: TypeDesc) match {
    case (JsBoolean(s), BooleanElement) => s
    case (JsString(s), IntElement) => s.toInt
    case (JsString(s), ShortElement) => s.toShort
    case (JsString(s), LongElement) => s.toLong
    case (JsString(s), ByteElement) => s.toByte
    case (JsString(s), FloatElement) => s.toFloat
    case (JsString(s), DoubleElement) => s.toDouble
    case (JsString(s), UnitElement) => ()
    case (JsString(s), CharElement) => s(0)
    case (JsString(s), StringElement) => s
    case (JsArray(Vector(jsA, jsB)), pe: PairElem[a,b]) =>
      (fromJsonValue(jsA, pe.eFst), fromJsonValue(jsB, pe.eSnd))
    case (JsArray(Vector(JsString("left"), jsLeft)), se: SumElem[a,b]) =>
      Left(fromJsonValue(jsLeft, se.eLeft))
    case (JsArray(Vector(JsString("right"), jsRight)), se: SumElem[a,b]) =>
      Right(fromJsonValue(jsRight, se.eRight))
    case _ =>
      ctx.!!!(s"""Don't know how to read json "$jsValue" into a value of type $e.""")
  }).asInstanceOf[A]

  object DeclaredUnOp {
    val unOpClass = classOf[UnOp[_, _]]
    val declaredOps = {
      val ops = for {
        int <- ctx.getClass.getInterfaces
        m <- int.getDeclaredMethods if unOpClass.isAssignableFrom(m.getReturnType)
      } yield {
        val op = m.invoke(ctx).asInstanceOf[UnOp[Any, Any]]
        (op.opName, op)
      }
      ops.toMap
    }

    def unapply(in: (String, Seq[Sym], Element)): Option[ApplyUnOp[_, _]] = declaredOps.get(in._1) match {
      case Some(op: UnOp[a, b]) =>
        Some(ApplyUnOp[a, b](op, in._2(0).asRep[a]))
      case _ => None
    }
  }

  object NumericBinOp {
    def unapply(in: (String, Seq[Sym], Element)): Option[ApplyBinOp[_, _]] = {
      val (opName: String, args: Seq[Sym], eRes) = in
      BinOps.get(opName) match {
        case Some(elems) => elems.get(eRes) match {
          case Some(op: BinOp[a, b]) =>
            Some(ApplyBinOp[a, b](op, args(0).asRep[a], args(1).asRep[a]))
          case _ => None
        }
        case None => None
      }
    }

    val BinOps: Map[String, Map[Element, BinOp[_, _]]] = Map(
      "+" -> Map[Element, BinOp[_, _]](
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
