package scalan.primitives

import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scalan._
import scalan.common.Lazy
import scalan.common.OverloadHack._
import scalan.compilation.{Compiler, GraphVizConfig, GraphVizExport}
import scalan.staged.Expressions

/**
 The code is taken from LMS and is used in Scalan with the same semantics
 in order to easily translate operations to the equivalents via LmsBridge.
 Their usage in Scalan is limited to be consistent with functional semantics of Scalan.
 Don't expect everything possible in LMS to be also possible in Scalan in the same way.
 There are changes in the code:
 - Sym -> Exp
 - Manifest -> Elem
 - infix -> implicit class
 - no SourceContext, withPos
 - mirroring implemented in Scalan way (though consistent with LMS)
 */

trait Structs { self: Scalan =>
  // TODO consider if T type parameter is needed here and for AbstractStruct
  // It's only useful if we'll have some static typing on structs later (Shapeless' records?)
  abstract class StructTag[T <: Struct](implicit val typeTag: TypeTag[T])
  case class SimpleTag[T <: Struct : TypeTag](name: String) extends StructTag[T]
  //  case class NestClassTag[C[_],T](elem: StructTag[T]) extends StructTag[C[T]]
  //  case class AnonTag[T](fields: RefinedManifest[T]) extends StructTag[T]
  //  case class MapTag[T]() extends StructTag[T]
  val defaultStructTag = SimpleTag[Struct]("Struct")

  protected def baseStructName(tag: StructTag[_]) = tag match {
    case `defaultStructTag` => ""
    case SimpleTag(name) => s"$name "
    // Intentionally no case _, add something here or override when extending StructTag!
  }

  trait Struct // TODO add tag/fields members from AbstractStruct here?

  case class StructElem[T <: Struct](structTag: StructTag[T], fields: Seq[(String, Elem[_])]) extends Elem[T] {
    override def isEntityType = fields.exists(_._2.isEntityType)
    lazy val tag = structTag.typeTag
    protected def getDefaultRep =
      struct(structTag, fields.map { case (fn,fe) => (fn, fe.defaultRepValue) }: _*)
    def get(fieldName: String): Option[Elem[_]] = fields.find(_._1 == fieldName).map(_._2)
    override def canEqual(other: Any) = other.isInstanceOf[StructElem[_]]
    def fieldElems: Seq[Elem[_]] = fields.map(_._2)
    def isEqualType(tuple: Seq[Elem[_]]) = {
      fields.length == tuple.length && fields.zip(tuple).forall { case ((fn,fe), e) => fe == e }
    }
    override def getName = {
      s"${baseStructName(structTag)}{${fields.map { case (fn,fe) => s"$fn: ${fe.name}"}.mkString("; ")}}"
    }
  }
  implicit def StructElemExtensions[T <: Struct](e: Elem[T]) = e.asInstanceOf[StructElem[T]]

  def structElement[T <: Struct](tag: StructTag[T], fields: Seq[(String, Elem[_])]): StructElem[T] =
    if (cacheElems)
      cachedElem[StructElem[T]](tag, fields)
    else
      StructElem(tag, fields)

  def structElement(fields: Seq[(String, Elem[_])]): StructElem[Struct] =
    structElement(defaultStructTag, fields)

  /**
    * Get tuple field name by index
    */
  private def tupleFN(fieldIndex: Int) = s"_$fieldIndex"

  def tupleStructElement(fieldElems: Elem[_]*)(implicit o: Overloaded1): StructElem[Struct] = {
    val fields = fieldElems.zipWithIndex.map { case (f, i) => tupleFN(i + 1) -> f }
    // TODO add tupleTag(n)?
    structElement(defaultStructTag, fields)
  }

  def tuple2StructElement[A:Elem, B:Elem]: StructElem[Struct] =
    tupleStructElement(element[A], element[B])

  def tuple3StructElement[A:Elem, B:Elem, C:Elem]: StructElem[Struct] =
    tupleStructElement(element[A], element[B], element[C])

  case class StructToPairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])
    extends Iso[Struct, (B1, B2)]()(tupleStructElement(iso1.eFrom, iso2.eFrom)) {
    implicit def eA1 = iso1.eFrom
    implicit def eA2 = iso2.eFrom
    implicit def eB1 = iso1.eTo
    implicit def eB2 = iso2.eTo
    lazy val eTo = element[(B1, B2)]

    override def from(p: Rep[(B1, B2)]) =
      struct(tupleFN(1) -> iso1.from(p._1), tupleFN(2) -> iso2.from(p._2))

    override def to(struct: Rep[Struct]) = {
      Pair(iso1.to(struct(1).asRep[A1]), iso2.to(struct(2).asRep[A2]))
    }
  }

  def structToPairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[Struct, (B1, B2)] =
    if (cacheIsos)
      cachedIso[StructToPairIso[A1, A2, B1, B2]](iso1, iso2)
    else
      StructToPairIso[A1, A2, B1, B2](iso1, iso2)

  def structToPairIso[A:Elem,B:Elem]: Iso[Struct, (A, B)] = structToPairIso[A,B,A,B](identityIso[A], identityIso[B])
  def structToPairIso[A,B](pe: Elem[(A,B)]): Iso[Struct, (A, B)] = structToPairIso[A,B](pe.eFst, pe.eSnd)

  class StructIso[S <: Struct, T <: Struct](override val eFrom: StructElem[S], val eTo: StructElem[T], itemIsos: Seq[Iso[_,_]])
      extends Iso[S, T]()(eFrom) {
    assert(eFrom.isEqualType(itemIsos.map(_.eFrom)))
    assert(eTo.isEqualType(itemIsos.map(_.eTo)))

    override def from(y: Rep[T]) = {
      val items = eFrom.fields.zip(eTo.fields).zip(itemIsos).map {
        case (((fnS, feS), (fnT, feT)), iso: Iso[s,t]) =>
          fnS -> iso.from(y(fnT).asRep[t])
      }
      struct(items).asRep[S]
    }
    override def to(x: Rep[S]) = {
      val items = eFrom.fields.zip(eTo.fields).zip(itemIsos).map {
        case (((fnS, feS), (fnT, feT)), iso: Iso[s,t]) =>
          fnT -> iso.to(x(fnS).asRep[s])
      }
      struct(items).asRep[T]
    }
  }

  implicit class StructOps(s: Rep[Struct]) {
    def apply(iField: Int): Rep[_] = field(s, iField)
    def apply(fieldName: String): Rep[_] = field(s, fieldName)
    def getChar(fieldName: String): Rep[Char] = field(s, fieldName).asRep[Char]
    def getFloat(fieldName: String): Rep[Float] = field(s, fieldName).asRep[Float]
    def getDouble(fieldName: String): Rep[Double] = field(s, fieldName).asRep[Double]
    def getInt(fieldName: String): Rep[Int] = field(s, fieldName).asRep[Int]
    def getLong(fieldName: String): Rep[Long] = field(s, fieldName).asRep[Long]
    def getString(fieldName: String): Rep[String] = field(s, fieldName).asRep[String]
    def getBoolean(fieldName: String): Rep[Boolean] = field(s, fieldName).asRep[Boolean]
    def getByte(fieldName: String): Rep[Byte] = field(s, fieldName).asRep[Byte]
    def getUnit(fieldName: String): Rep[Unit] = field(s, fieldName).asRep[Unit]
    def getShort(fieldName: String): Rep[Short] = field(s, fieldName).asRep[Short]
  }

  def struct(fields: (String, Rep[Any])*)(implicit o: Overloaded1): Rep[Struct] = struct(fields)
  def struct(fields: Seq[(String, Rep[Any])]): Rep[Struct] = struct(defaultStructTag, fields)
  def struct[T <: Struct](tag: StructTag[T], fields: (String, Rep[Any])*)(implicit o: Overloaded1): Rep[T] =
    struct(tag, fields)
  def struct[T <: Struct](tag: StructTag[T], fields: Seq[(String, Rep[Any])]): Rep[T]
  def tupleStruct(items: Rep[_]*): Rep[Struct] = {
    val fields = items.zipWithIndex.map { case (f, i) => tupleFN(i + 1) -> f }
    struct(defaultStructTag, fields)
  }
  def field(struct: Rep[Struct], field: String): Rep[_]
  def field(struct: Rep[Struct], fieldIndex: Int): Rep[_] = field(struct, tupleFN(fieldIndex))
  def fields(struct: Rep[Struct], fields: Seq[String]): Rep[Struct]

  case class Link(field: String, nestedField: String, nestedElem: Elem[_], flatIndex: Int)

  class FlatteningIso[T <: Struct](val eTo: StructElem[T], val flatIsos: Map[String, Iso[_,_]], links: Seq[Link])
      extends Iso[Struct,T]()(tupleStructElement(links.map(_.nestedElem): _*)) {

    val groups = links.groupBy(_.field)

    def to(x: Rep[Struct]) = {
      val items = eTo.fields.map { case (fn, fe) =>
        val g = groups(fn)
        flatIsos.get(fn) match {
          case Some(iso: Iso[a, _]) =>
            val projectedStruct = struct(g.map(link => (link.nestedField -> x(link.flatIndex))): _*)
            val s = iso.to(projectedStruct.asRep[a])
            (fn -> s)
          case _ =>
            assert(g.length == 1, s"Many fields $g can't relate to the single field $fn without iso")
            (fn -> x(g(0).flatIndex))
        }
      }
      struct(eTo.structTag, items: _*)
    }

    def from(y: Rep[T]) = {
      val items = eTo.fields.flatMap { case (fn, fe) =>
        val g = groups(fn)
        flatIsos.get(fn) match {
          case Some(iso: Iso[_, a]) =>
            val nestedStruct = iso.from(y(fn).asRep[a]).asRep[Struct]
            // nestedStruct is guaranteed to be a Rep[Struct], because iso can be either IdentityIso on a struct or FlatteningIso
            g.map { link =>
              tupleFN(link.flatIndex) -> nestedStruct(link.nestedField)
            }
          case _ =>
            List(tupleFN(g(0).flatIndex) -> y(fn))
        }
      }
      struct(items: _*)
    }
  }

  /**
   * Flattens all subtrees of structs in [[eTo]].
   * Types other than structs are considered either as internal nodes of as leaves.
   * @param eTo descriptor of struct type
   * @return an isomorphism [[i]] in which [[eTo]] is given by param and [[eFrom]] is flattened [[eTo]] preserving
   *         related order of the components
   */
  def getFlatteningIso[T](e: Elem[T]): Iso[_,T] = e match {
    // a == T, but Scala can't infer the type bound if T is used below
    case se: StructElem[a] @unchecked =>
      flatteningIso(se).asInstanceOf[Iso[_, T]] match {
        case idIso: IdentityIso[T] @unchecked =>
          idIso
        case flatIso: FlatteningIso[T] @unchecked =>
          flatIso.eFrom match {
            // TODO Actually, we currently know s == Struct. Is extra complexity needed?
            case eFrom: StructElem[s] =>
              val isos = eFrom.fields.map { case (fn,fe) => (fn, buildIso(fe, flatteningBuilder)) }
              val eFromNew = structElement(isos.map { case (fn, iso) => fn -> iso.eFrom })
              val sIso = new StructIso(eFromNew, eFrom, isos.map(_._2))
              sIso >> flatIso.asInstanceOf[Iso[s,T]]
          }
      }
    case _ =>
      buildIso(e, flatteningBuilder)
  }

  val flatteningBuilder = new IsoBuilder { def apply[S](e: Elem[S]) = getFlatteningIso(e) }

  def flatteningIso[T <: Struct](eTo: StructElem[T]): Iso[_, T] = {
    val flatIsos: Map[String, Iso[_, _]] = eTo.fields.collect {
      case (fn, fe: StructElem[_]) => (fn, flatteningIso(fe))
    }.toMap

    if (flatIsos.isEmpty)
      return identityIso(eTo)

    // relate resulting field types by original field name
    val fromFields = eTo.fields.flatMap {
      case (fn, fe) =>
        flatIsos.get(fn) match {
          case Some(iso) =>
            iso.eFrom match {
              case flatElem: StructElem[_] =>
                flatElem.fields.map { case (nestedName, nestedE) => (fn, nestedName -> nestedE) }
              case _ => !!!(s"StructElem is expected as eFrom of flattened Iso $iso")
            }
          case None => List((fn, "" -> fe))
        }
    }

    val links = fromFields.zipWithIndex.map {
      case ((fn, (nestedN, nestedE)), i) => Link(fn, nestedN, nestedE, i + 1)
    }

    new FlatteningIso(eTo, flatIsos, links)
  }

  def getStructToPairsIso[T](implicit e: Elem[T]): Iso[_,T] = (e match {
    case pe: PairElem[a,b] =>
      val iso1 = getStructToPairsIso(pe.eFst)
      val iso2 = getStructToPairsIso(pe.eSnd)
      val res = structToPairIso(iso1, iso2)
      res
    case _ =>
      buildIso(e, new IsoBuilder {
        def apply[S](e: Elem[S]) = {
          getStructToPairsIso(e)
        }
      })
  }).asInstanceOf[Iso[_,T]]


  def getStructWrapperIso[T](implicit e: Elem[T]): Iso[_,T] = {
    getStructToPairsIso(e) match {
      case iso: Iso[s,T] @unchecked =>
        val flatIso = getFlatteningIso[s](iso.eFrom)
        flatIso >> iso
    }
  }

  def structWrapper[A:Elem,B:Elem](f: Rep[A => B]): Rep[Any => Any] = {
    val inIso = getStructWrapperIso[A].asIso[Any,A]
    val outIso = getStructWrapperIso[B].asIso[Any,B]
    fun({ (in: Rep[Any]) =>
      outIso.from(f(inIso.to(in)))
    })(Lazy(inIso.eFrom), outIso.eFrom)
  }
  def structWrapperIn[A:Elem,B:Elem](f: Rep[A => B]): Rep[Any => B] = {
    val inIso = getStructWrapperIso[A].asIso[Any,A]
    fun({ (in: Rep[Any]) =>
      f(inIso.to(in))
    })(Lazy(inIso.eFrom), element[B])
  }
  def structWrapperOut[A:Elem,B:Elem](f: Rep[A => B]): Rep[A => Any] = {
    val outIso = getStructWrapperIso[B].asIso[Any,B]
    fun({ (in: Rep[A]) =>
      outIso.from(f(in))
    })(Lazy(element[A]), outIso.eFrom)
  }

}

trait StructsSeq extends Structs { self: ScalanSeq =>
  case class StructSeq[T <: Struct](tag: StructTag[T], fields: Seq[(String, Rep[Any])]) extends Struct

  def struct[T <: Struct](tag: StructTag[T], fields: Seq[(String, Rep[Any])]): Rep[T] =
    StructSeq(tag, fields).asRep[T]
  def field(struct: Rep[Struct], field: String): Rep[_] =
    struct.asInstanceOf[StructSeq[_]].fields.find(_._1 == field) match {
      case Some((_, value)) => value
      case None => !!!(s"Field $field not found in structure $struct")
    }
  def fields(struct: Rep[Struct], fields: Seq[String]): Rep[Struct] = {
    val StructSeq(tag, fieldsInStruct) = struct
    StructSeq(tag, fieldsInStruct.filter(fields.contains))
  }
}

trait StructsExp extends Expressions with Structs with EffectsExp with ViewsExp
    with GraphVizExport { self: ScalanExp =>


  abstract class AbstractStruct[T <: Struct] extends Def[T] {
    def tag: StructTag[T]
    def fields: Seq[(String, Rep[Any])]
    lazy val selfType = structElement(tag, fields.map { case (name, value) => (name, value.elem) })
  }

  abstract class AbstractField[T] extends Def[T] {
    def struct: Rep[Struct]
    def field: String
    lazy val selfType = Struct.getFieldElem(struct, field).asElem[T]
  }

  object Struct {
    def getFieldElem(struct: Rep[Struct], fn: String): Elem[_] = struct.elem match {
      case se: StructElem[_] =>
        se.get(fn).get
      case _ => !!!(s"Symbol with StructElem expected but found ${struct.elem}", struct)
    }

    def unapply[T <: Struct](d: Def[T]) = unapplyStruct(d)
  }

  def unapplyStruct[T <: Struct](d: Def[T]): Option[(StructTag[T], Seq[(String, Rep[Any])])] = d match {
    case s: AbstractStruct[T] => Some((s.tag, s.fields))
    case _ => None
  }

  object Field {
    def unapply[T](d: Def[T]) = unapplyField(d)
  }

  def unapplyField[T](d: Def[T]): Option[(Rep[Struct], String)] = d match {
    case f: AbstractField[T] => Some((f.struct, f.field))
    case _ => None
  }

  case class SimpleStruct[T <: Struct](tag: StructTag[T], fields: Seq[(String, Rep[Any])]) extends AbstractStruct[T]
  case class FieldApply[T](struct: Rep[Struct], field: String) extends AbstractField[T]
  case class ProjectionStruct(struct: Rep[Struct], outFields: Seq[String]) extends AbstractStruct[Struct] {
    def tag = defaultStructTag
    val fields = outFields.map(fn => (fn, field(struct, fn)))
  }

  def struct[T <: Struct](tag: StructTag[T], fields: Seq[(String, Rep[Any])]): Rep[T] = SimpleStruct(tag, fields)
  def field(struct: Rep[Struct], field: String): Rep[_] = FieldApply[Any](struct, field) // TODO Any?
  def fields(struct: Rep[Struct], fields: Seq[String]): Rep[Struct] = ProjectionStruct(struct, fields)

  override def syms(e: Any): List[Exp[Any]] = e match {
    case s: ProjectionStruct => syms(s.struct)
    case s: AbstractStruct[_] => s.fields.flatMap(e => this.syms(e._2)).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case s: ProjectionStruct => symsFreq(s.struct)
    case s: AbstractStruct[_] => s.fields.flatMap(e => symsFreq(e._2)).toList
    case _ => super.symsFreq(e)
  }

  override def effectSyms(e: Any): List[Exp[Any]] = e match {
    case s: ProjectionStruct => effectSyms(s.struct)
    case s: AbstractStruct[_] => s.fields.flatMap(e => effectSyms(e._2)).toList
    case _ => super.effectSyms(e)
  }

  override def readSyms(e: Any): List[Exp[Any]] = e match {
    case s: AbstractStruct[_] => Nil //struct creation doesn't de-reference any of its inputs
    case _ => super.readSyms(e)
  }

  override def aliasSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => Nil
    case FieldApply(s,x) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => fields.collect { case (k, v: Sym[_]) => v }.toList
    case FieldApply(s,x) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => Nil
    case FieldApply(s,x) => syms(s)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => Nil
    case FieldApply(s,x) => Nil
    case _ => super.copySyms(e)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case SimpleStruct(tag, fields) =>
      s"${baseStructName(tag)}{${fields.map { case (fn, s) => s"$fn:$s" }.mkString("; ")}}"
    case ProjectionStruct(struct, outs) => s"$struct.{${outs.mkString(",")}}"
    case FieldApply(struct, fn) => s"$struct.[$fn]"
    case _ => super.formatDef(d)
  }

  case class ViewStruct[A, B](source: Exp[A])(val iso: Iso[A, B])
    extends View[A, B] {
    override def toString = s"ViewStruct[${iso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewStruct[_, _] => source == v.source && iso.eTo == v.iso.eTo
      case _ => false
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewStruct[a, b]) =>
      Some((view.source, view.iso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  object FieldGet {
    def unapply[T](d: FieldApply[T]): Option[Exp[T]] = d match {
      case FieldApply(Def(SimpleStruct(_, fs)), fn) =>
        val optItem = fs.find { case (n, _) => n == fn }
        optItem.map(_._2.asRep[T])
      case _ => None
    }
  }

  def shouldUnpackTuples = currentPass.config.shouldUnpackTuples

  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
    case FieldGet(v) if shouldUnpackTuples => v
    case _ => super.rewriteDef(d)
  }
}

trait StructsCompiler[ScalanCake <: ScalanCtxExp with StructsExp] extends Compiler[ScalanCake] {
  import scalan._

  object StructsRewriter extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] = (x match {
      case Def(FieldGet(v)) => v
      case _ => x
    }).asRep[T]
  }

  override def graphPasses(compilerConfig: CompilerConfig) =
    super.graphPasses(compilerConfig) ++
      Seq(AllInvokeEnabler,
          constantPass(StructsPass(DefaultMirror, StructsRewriter)))

  case class StructsPass(mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def name = "structs"
    override val config = PassConfig(shouldUnpackTuples = true)
    def apply(graph: PGraph): PGraph = {
      graph.transform(mirror, rewriter, MapTransformer.Empty)
    }
  }

}


