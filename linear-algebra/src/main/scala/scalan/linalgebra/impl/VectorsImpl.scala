package scalan.linalgebra

import scalan._
import scalan.collections.{CollectionsDsl, CollectionsDslSeq, CollectionsDslExp}
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait VectorsAbs extends scalan.ScalanDsl with Vectors {
  self: VectorsDsl =>

  // single proxy for each type family
  implicit def proxyVector[T](p: Rep[Vector[T]]): Vector[T] = {
    proxyOps[Vector[T]](p)(scala.reflect.classTag[Vector[T]])
  }

  // familyElem
  class VectorElem[T, To <: Vector[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[Vector[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Vector[T]] => convertVector(x) }
      tryConvert(element[Vector[T]], this, x, conv)
    }

    def convertVector(x: Rep[Vector[T]]): Rep[To] = {
      x.selfType1 match {
        case _: VectorElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have VectorElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def vectorElement[T](implicit eT: Elem[T]): Elem[Vector[T]] =
    cachedElem[VectorElem[T, Vector[T]]](eT)

  implicit case object VectorCompanionElem extends CompanionElem[VectorCompanionAbs] {
    lazy val tag = weakTypeTag[VectorCompanionAbs]
    protected def getDefaultRep = Vector
  }

  abstract class VectorCompanionAbs extends CompanionDef[VectorCompanionAbs] {
    def selfType = VectorCompanionElem
    override def toString = "Vector"
  }
  def Vector: Rep[VectorCompanionAbs]
  implicit def proxyVectorCompanionAbs(p: Rep[VectorCompanionAbs]): VectorCompanionAbs =
    proxyOps[VectorCompanionAbs](p)

  abstract class AbsDenseVector[T]
      (items: Rep[Collection[T]])(implicit eT: Elem[T])
    extends DenseVector[T](items) with Def[DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]]
  }
  // elem for concrete class
  class DenseVectorElem[T](val iso: Iso[DenseVectorData[T], DenseVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, DenseVector[T]]
    with ConcreteElem[DenseVectorData[T], DenseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = DenseVector(x.items)
    override def getDefaultRep = DenseVector(element[Collection[T]].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DenseVector[T]]
    }
  }

  // state representation type
  type DenseVectorData[T] = Collection[T]

  // 3) Iso for concrete class
  class DenseVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[DenseVectorData[T], DenseVector[T]] with Def[DenseVectorIso[T]] {
    override def from(p: Rep[DenseVector[T]]) =
      p.items
    override def to(p: Rep[Collection[T]]) = {
      val items = p
      DenseVector(items)
    }
    lazy val eFrom = element[Collection[T]]
    lazy val eTo = new DenseVectorElem[T](self)
    lazy val selfType = new DenseVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class DenseVectorIsoElem[T](eT: Elem[T]) extends Elem[DenseVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new DenseVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DenseVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class DenseVectorCompanionAbs extends CompanionDef[DenseVectorCompanionAbs] with DenseVectorCompanion {
    def selfType = DenseVectorCompanionElem
    override def toString = "DenseVector"

    def apply[T](items: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
      mkDenseVector(items)

    def unapply[T](p: Rep[Vector[T]]) = unmkDenseVector(p)
  }
  lazy val DenseVectorRep: Rep[DenseVectorCompanionAbs] = new DenseVectorCompanionAbs
  lazy val DenseVector: DenseVectorCompanionAbs = proxyDenseVectorCompanion(DenseVectorRep)
  implicit def proxyDenseVectorCompanion(p: Rep[DenseVectorCompanionAbs]): DenseVectorCompanionAbs = {
    proxyOps[DenseVectorCompanionAbs](p)
  }

  implicit case object DenseVectorCompanionElem extends CompanionElem[DenseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[DenseVectorCompanionAbs]
    protected def getDefaultRep = DenseVector
  }

  implicit def proxyDenseVector[T](p: Rep[DenseVector[T]]): DenseVector[T] =
    proxyOps[DenseVector[T]](p)

  implicit class ExtendedDenseVector[T](p: Rep[DenseVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[DenseVectorData[T]] = isoDenseVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoDenseVector[T](implicit eT: Elem[T]): Iso[DenseVectorData[T], DenseVector[T]] =
    reifyObject(new DenseVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkDenseVector[T](items: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]]
  def unmkDenseVector[T](p: Rep[Vector[T]]): Option[(Rep[Collection[T]])]

  abstract class AbsConstVector[T]
      (item: Rep[T], length: Rep[Int])(implicit eT: Elem[T])
    extends ConstVector[T](item, length) with Def[ConstVector[T]] {
    lazy val selfType = element[ConstVector[T]]
  }
  // elem for concrete class
  class ConstVectorElem[T](val iso: Iso[ConstVectorData[T], ConstVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, ConstVector[T]]
    with ConcreteElem[ConstVectorData[T], ConstVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Vector to ConstVector: missing fields List(item)")
    override def getDefaultRep = ConstVector(element[T].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ConstVector[T]]
    }
  }

  // state representation type
  type ConstVectorData[T] = (T, Int)

  // 3) Iso for concrete class
  class ConstVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[ConstVectorData[T], ConstVector[T]] with Def[ConstVectorIso[T]] {
    override def from(p: Rep[ConstVector[T]]) =
      (p.item, p.length)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(item, length) = p
      ConstVector(item, length)
    }
    lazy val eFrom = pairElement(element[T], element[Int])
    lazy val eTo = new ConstVectorElem[T](self)
    lazy val selfType = new ConstVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class ConstVectorIsoElem[T](eT: Elem[T]) extends Elem[ConstVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ConstVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ConstVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class ConstVectorCompanionAbs extends CompanionDef[ConstVectorCompanionAbs] with ConstVectorCompanion {
    def selfType = ConstVectorCompanionElem
    override def toString = "ConstVector"
    def apply[T](p: Rep[ConstVectorData[T]])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      isoConstVector(eT).to(p)
    def apply[T](item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      mkConstVector(item, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkConstVector(p)
  }
  lazy val ConstVectorRep: Rep[ConstVectorCompanionAbs] = new ConstVectorCompanionAbs
  lazy val ConstVector: ConstVectorCompanionAbs = proxyConstVectorCompanion(ConstVectorRep)
  implicit def proxyConstVectorCompanion(p: Rep[ConstVectorCompanionAbs]): ConstVectorCompanionAbs = {
    proxyOps[ConstVectorCompanionAbs](p)
  }

  implicit case object ConstVectorCompanionElem extends CompanionElem[ConstVectorCompanionAbs] {
    lazy val tag = weakTypeTag[ConstVectorCompanionAbs]
    protected def getDefaultRep = ConstVector
  }

  implicit def proxyConstVector[T](p: Rep[ConstVector[T]]): ConstVector[T] =
    proxyOps[ConstVector[T]](p)

  implicit class ExtendedConstVector[T](p: Rep[ConstVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[ConstVectorData[T]] = isoConstVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConstVector[T](implicit eT: Elem[T]): Iso[ConstVectorData[T], ConstVector[T]] =
    reifyObject(new ConstVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkConstVector[T](item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]]
  def unmkConstVector[T](p: Rep[Vector[T]]): Option[(Rep[T], Rep[Int])]

  abstract class AbsSparseVector[T]
      (nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length) with Def[SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]]
  }
  // elem for concrete class
  class SparseVectorElem[T](val iso: Iso[SparseVectorData[T], SparseVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, SparseVector[T]]
    with ConcreteElem[SparseVectorData[T], SparseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = SparseVector(x.nonZeroIndices, x.nonZeroValues, x.length)
    override def getDefaultRep = SparseVector(element[Collection[Int]].defaultRepValue, element[Collection[T]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVector[T]]
    }
  }

  // state representation type
  type SparseVectorData[T] = (Collection[Int], (Collection[T], Int))

  // 3) Iso for concrete class
  class SparseVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[SparseVectorData[T], SparseVector[T]] with Def[SparseVectorIso[T]] {
    override def from(p: Rep[SparseVector[T]]) =
      (p.nonZeroIndices, p.nonZeroValues, p.length)
    override def to(p: Rep[(Collection[Int], (Collection[T], Int))]) = {
      val Pair(nonZeroIndices, Pair(nonZeroValues, length)) = p
      SparseVector(nonZeroIndices, nonZeroValues, length)
    }
    lazy val eFrom = pairElement(element[Collection[Int]], pairElement(element[Collection[T]], element[Int]))
    lazy val eTo = new SparseVectorElem[T](self)
    lazy val selfType = new SparseVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class SparseVectorIsoElem[T](eT: Elem[T]) extends Elem[SparseVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SparseVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class SparseVectorCompanionAbs extends CompanionDef[SparseVectorCompanionAbs] with SparseVectorCompanion {
    def selfType = SparseVectorCompanionElem
    override def toString = "SparseVector"
    def apply[T](p: Rep[SparseVectorData[T]])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      isoSparseVector(eT).to(p)
    def apply[T](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      mkSparseVector(nonZeroIndices, nonZeroValues, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkSparseVector(p)
  }
  lazy val SparseVectorRep: Rep[SparseVectorCompanionAbs] = new SparseVectorCompanionAbs
  lazy val SparseVector: SparseVectorCompanionAbs = proxySparseVectorCompanion(SparseVectorRep)
  implicit def proxySparseVectorCompanion(p: Rep[SparseVectorCompanionAbs]): SparseVectorCompanionAbs = {
    proxyOps[SparseVectorCompanionAbs](p)
  }

  implicit case object SparseVectorCompanionElem extends CompanionElem[SparseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[SparseVectorCompanionAbs]
    protected def getDefaultRep = SparseVector
  }

  implicit def proxySparseVector[T](p: Rep[SparseVector[T]]): SparseVector[T] =
    proxyOps[SparseVector[T]](p)

  implicit class ExtendedSparseVector[T](p: Rep[SparseVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[SparseVectorData[T]] = isoSparseVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSparseVector[T](implicit eT: Elem[T]): Iso[SparseVectorData[T], SparseVector[T]] =
    reifyObject(new SparseVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkSparseVector[T](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]]
  def unmkSparseVector[T](p: Rep[Vector[T]]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int])]

  abstract class AbsSparseVector1[T]
      (nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T])
    extends SparseVector1[T](nonZeroItems, length) with Def[SparseVector1[T]] {
    lazy val selfType = element[SparseVector1[T]]
  }
  // elem for concrete class
  class SparseVector1Elem[T](val iso: Iso[SparseVector1Data[T], SparseVector1[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, SparseVector1[T]]
    with ConcreteElem[SparseVector1Data[T], SparseVector1[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = SparseVector1(x.nonZeroItems, x.length)
    override def getDefaultRep = SparseVector1(element[Collection[(Int, T)]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVector1[T]]
    }
  }

  // state representation type
  type SparseVector1Data[T] = (Collection[(Int, T)], Int)

  // 3) Iso for concrete class
  class SparseVector1Iso[T](implicit eT: Elem[T])
    extends EntityIso[SparseVector1Data[T], SparseVector1[T]] with Def[SparseVector1Iso[T]] {
    override def from(p: Rep[SparseVector1[T]]) =
      (p.nonZeroItems, p.length)
    override def to(p: Rep[(Collection[(Int, T)], Int)]) = {
      val Pair(nonZeroItems, length) = p
      SparseVector1(nonZeroItems, length)
    }
    lazy val eFrom = pairElement(element[Collection[(Int, T)]], element[Int])
    lazy val eTo = new SparseVector1Elem[T](self)
    lazy val selfType = new SparseVector1IsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class SparseVector1IsoElem[T](eT: Elem[T]) extends Elem[SparseVector1Iso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SparseVector1Iso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVector1Iso[T]]
    }
  }
  // 4) constructor and deconstructor
  class SparseVector1CompanionAbs extends CompanionDef[SparseVector1CompanionAbs] with SparseVector1Companion {
    def selfType = SparseVector1CompanionElem
    override def toString = "SparseVector1"
    def apply[T](p: Rep[SparseVector1Data[T]])(implicit eT: Elem[T]): Rep[SparseVector1[T]] =
      isoSparseVector1(eT).to(p)
    def apply[T](nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector1[T]] =
      mkSparseVector1(nonZeroItems, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkSparseVector1(p)
  }
  lazy val SparseVector1Rep: Rep[SparseVector1CompanionAbs] = new SparseVector1CompanionAbs
  lazy val SparseVector1: SparseVector1CompanionAbs = proxySparseVector1Companion(SparseVector1Rep)
  implicit def proxySparseVector1Companion(p: Rep[SparseVector1CompanionAbs]): SparseVector1CompanionAbs = {
    proxyOps[SparseVector1CompanionAbs](p)
  }

  implicit case object SparseVector1CompanionElem extends CompanionElem[SparseVector1CompanionAbs] {
    lazy val tag = weakTypeTag[SparseVector1CompanionAbs]
    protected def getDefaultRep = SparseVector1
  }

  implicit def proxySparseVector1[T](p: Rep[SparseVector1[T]]): SparseVector1[T] =
    proxyOps[SparseVector1[T]](p)

  implicit class ExtendedSparseVector1[T](p: Rep[SparseVector1[T]])(implicit eT: Elem[T]) {
    def toData: Rep[SparseVector1Data[T]] = isoSparseVector1(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSparseVector1[T](implicit eT: Elem[T]): Iso[SparseVector1Data[T], SparseVector1[T]] =
    reifyObject(new SparseVector1Iso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkSparseVector1[T](nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector1[T]]
  def unmkSparseVector1[T](p: Rep[Vector[T]]): Option[(Rep[Collection[(Int, T)]], Rep[Int])]

  registerModule(Vectors_Module)
}

// Seq -----------------------------------
trait VectorsSeq extends scalan.ScalanDslStd with VectorsDsl {
  self: VectorsDslSeq =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs {
  }

  case class SeqDenseVector[T]
      (override val items: Rep[Collection[T]])(implicit eT: Elem[T])
    extends AbsDenseVector[T](items) {
  }

  def mkDenseVector[T]
    (items: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
    new SeqDenseVector[T](items)
  def unmkDenseVector[T](p: Rep[Vector[T]]) = p match {
    case p: DenseVector[T] @unchecked =>
      Some((p.items))
    case _ => None
  }

  case class SeqConstVector[T]
      (override val item: Rep[T], override val length: Rep[Int])(implicit eT: Elem[T])
    extends AbsConstVector[T](item, length) {
  }

  def mkConstVector[T]
    (item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
    new SeqConstVector[T](item, length)
  def unmkConstVector[T](p: Rep[Vector[T]]) = p match {
    case p: ConstVector[T] @unchecked =>
      Some((p.item, p.length))
    case _ => None
  }

  case class SeqSparseVector[T]
      (override val nonZeroIndices: Rep[Collection[Int]], override val nonZeroValues: Rep[Collection[T]], override val length: Rep[Int])(implicit eT: Elem[T])
    extends AbsSparseVector[T](nonZeroIndices, nonZeroValues, length) {
  }

  def mkSparseVector[T]
    (nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
    new SeqSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T](p: Rep[Vector[T]]) = p match {
    case p: SparseVector[T] @unchecked =>
      Some((p.nonZeroIndices, p.nonZeroValues, p.length))
    case _ => None
  }

  case class SeqSparseVector1[T]
      (override val nonZeroItems: Coll[(Int, T)], override val length: Rep[Int])(implicit eT: Elem[T])
    extends AbsSparseVector1[T](nonZeroItems, length) {
  }

  def mkSparseVector1[T]
    (nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector1[T]] =
    new SeqSparseVector1[T](nonZeroItems, length)
  def unmkSparseVector1[T](p: Rep[Vector[T]]) = p match {
    case p: SparseVector1[T] @unchecked =>
      Some((p.nonZeroItems, p.length))
    case _ => None
  }
}

// Exp -----------------------------------
trait VectorsExp extends scalan.ScalanDslExp with VectorsDsl {
  self: VectorsDslExp =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs {
  }

  case class ExpDenseVector[T]
      (override val items: Rep[Collection[T]])(implicit eT: Elem[T])
    extends AbsDenseVector[T](items)

  object DenseVectorMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[DenseVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_value_+^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_value_-^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_value_*^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[Double], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[DenseVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(num, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, num)).asInstanceOf[Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DenseVectorCompanionMethods {
    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem == DenseVectorCompanionElem && method.getName == "zero" =>
          Some(len).asInstanceOf[Option[Rep[Int] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromSparseData {
      def unapply(d: Def[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(nonZeroIndices, nonZeroValues, length, _*), _) if receiver.elem == DenseVectorCompanionElem && method.getName == "fromSparseData" =>
          Some((nonZeroIndices, nonZeroValues, length)).asInstanceOf[Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkDenseVector[T]
    (items: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
    new ExpDenseVector[T](items)
  def unmkDenseVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DenseVectorElem[T] @unchecked =>
      Some((p.asRep[DenseVector[T]].items))
    case _ =>
      None
  }

  case class ExpConstVector[T]
      (override val item: Rep[T], override val length: Rep[Int])(implicit eT: Elem[T])
    extends AbsConstVector[T](item, length)

  object ConstVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[ConstVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_value_+^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_value_-^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_value_*^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[Double], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[ConstVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(num, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, num)).asInstanceOf[Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ConstVectorCompanionMethods {
    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem == ConstVectorCompanionElem && method.getName == "zero" =>
          Some(len).asInstanceOf[Option[Rep[Int] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromSparseData {
      def unapply(d: Def[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(nonZeroIndices, nonZeroValues, length, _*), _) if receiver.elem == ConstVectorCompanionElem && method.getName == "fromSparseData" =>
          Some((nonZeroIndices, nonZeroValues, length)).asInstanceOf[Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkConstVector[T]
    (item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
    new ExpConstVector[T](item, length)
  def unmkConstVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConstVectorElem[T] @unchecked =>
      Some((p.asRep[ConstVector[T]].item, p.asRep[ConstVector[T]].length))
    case _ =>
      None
  }

  case class ExpSparseVector[T]
      (override val nonZeroIndices: Rep[Collection[Int]], override val nonZeroValues: Rep[Collection[T]], override val length: Rep[Int])(implicit eT: Elem[T])
    extends AbsSparseVector[T](nonZeroIndices, nonZeroValues, length)

  object SparseVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[SparseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[SparseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[SparseVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[SparseVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_value_+^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_value_-^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_value_*^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[Double], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(num, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, num)).asInstanceOf[Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[SparseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SparseVectorCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, n, _*), _) if receiver.elem == SparseVectorCompanionElem && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, n)).asInstanceOf[Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_SparseVectorCompanion_apply_nonZeroItems {
      def unapply(d: Def[_]): Option[(Rep[Collection[(Int, T)]], Rep[Int], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(nonZeroItems, length, n, _*), _) if receiver.elem == SparseVectorCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "SparseVectorCompanion_apply_nonZeroItems" } =>
          Some((nonZeroItems, length, n)).asInstanceOf[Option[(Rep[Collection[(Int, T)]], Rep[Int], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[(Int, T)]], Rep[Int], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem == SparseVectorCompanionElem && method.getName == "zero" =>
          Some(len).asInstanceOf[Option[Rep[Int] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromSparseData {
      def unapply(d: Def[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(nonZeroIndices, nonZeroValues, length, _*), _) if receiver.elem == SparseVectorCompanionElem && method.getName == "fromSparseData" =>
          Some((nonZeroIndices, nonZeroValues, length)).asInstanceOf[Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkSparseVector[T]
    (nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
    new ExpSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SparseVectorElem[T] @unchecked =>
      Some((p.asRep[SparseVector[T]].nonZeroIndices, p.asRep[SparseVector[T]].nonZeroValues, p.asRep[SparseVector[T]].length))
    case _ =>
      None
  }

  case class ExpSparseVector1[T]
      (override val nonZeroItems: Coll[(Int, T)], override val length: Rep[Int])(implicit eT: Elem[T])
    extends AbsSparseVector1[T](nonZeroItems, length)

  object SparseVector1Methods {
    object items {
      def unapply(d: Def[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector1[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector1[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector1[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[SparseVector1[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[SparseVector1[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_value_+^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_value_-^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_value_*^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[Double], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[Double], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[Double], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[SparseVector1[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(num, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, num)).asInstanceOf[Option[(Rep[SparseVector1[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector1[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SparseVector1CompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, n, _*), _) if receiver.elem == SparseVector1CompanionElem && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, n)).asInstanceOf[Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_SparseVector1Companion_apply_nonZeroItems {
      def unapply(d: Def[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(nonZeroIndices, nonZeroValues, length, n, _*), _) if receiver.elem == SparseVector1CompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "SparseVector1Companion_apply_nonZeroItems" } =>
          Some((nonZeroIndices, nonZeroValues, length, n)).asInstanceOf[Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem == SparseVector1CompanionElem && method.getName == "zero" =>
          Some(len).asInstanceOf[Option[Rep[Int] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromSparseData {
      def unapply(d: Def[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(nonZeroIndices, nonZeroValues, length, _*), _) if receiver.elem == SparseVector1CompanionElem && method.getName == "fromSparseData" =>
          Some((nonZeroIndices, nonZeroValues, length)).asInstanceOf[Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkSparseVector1[T]
    (nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector1[T]] =
    new ExpSparseVector1[T](nonZeroItems, length)
  def unmkSparseVector1[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SparseVector1Elem[T] @unchecked =>
      Some((p.asRep[SparseVector1[T]].nonZeroItems, p.asRep[SparseVector1[T]].length))
    case _ =>
      None
  }

  object VectorMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object items {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zeroValue {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "zeroValue" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[Vector[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[Vector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_collection_+^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_collection" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_value_+^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_collection_-^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_collection" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_value_-^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_collection_*^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_collection" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_value_*^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, f, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$div$up" =>
          Some((receiver, other, f)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[Double], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[Double], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[Double], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(num, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "euclideanNorm" =>
          Some((receiver, num)).asInstanceOf[Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[Vector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroesLength {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroesLength" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Vectors_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAOVXTWwbRRR+6/gnttP0h6YUREQIhooK4gSBesihCk4KQSaJsmmFTFU0Xk+cLbOzm51xZHOoOFUIbogrEpW4IPWCOKFKFRJCQhw4IYTUQ0+cSlHVAxUHEG/Gu+t1bDdJRatI+DDanX3z3pvv+96b8dXbkBI+PC8swgifcqgkU6Z+nhOyYC5wacvWW26tweg8XT/ufvv5zJdPfpOAgxVIbxAxL1gFsu2HhaYXPZt0swxZwi0qpOsLCc+UdYSi5TJGLWm7vGg7TkOSKqPFsi3kbBmSVbfW2oRLYJThkOVyy6eSmiVGhKAimB+mKiM7es/q99ay14nBi2oXxdgu1nxiS0wfYxxq269Sz2xxl7ccCaNBasueSgttMrbjub4MQ2TQ3YZbC1+TnOAEHClfJFukiCHqRVP6Nq/jyrxHrPdInS6hiTJPYsKCsvW1lqffh8qQE3QTAVp0PKZnmh4AIAMv6ySmOvhMRfhMKXwKJvVtwuz3ifq44rvNFrR/xhBA00MXL+7gIvRAF3it8NF56517Zt5JqMVNlUpG7zCNjp4eoAZNBeL4w+on4u7rV04lIFeBnC3mqkL6xJJxygO08oRzV+qcIwCJX0e2JgexpaPMoc02SWQt1/EIR08BlCPIE7MtWypjNTcSsDMA+oz0aGhqND0j2u/EgP1q3ZQIYyu3nnjpud8X3k5AojtEFl2aKHw/dCohfQ7BDwBI6/GgBGNNI6yGbLMzZu4TPILhxK0/at9Pw/lEBF4Qa3d8oYuU+PWX/M8vnE7AcEWr+wwj9QriJxYYdZb9kstlBYbdLeq3v2S2CFNPffnL1Og6aTAZoBqHYwjhkDAxsA49qrCa1Zo3QgDybdkuuZwWzqwU/jR//PSqUqUPI+0v7cL8xz71943RdakFKyFlS+qIEN8hLOhuxHOlqAx2RUWHkFw7quk69PDkXfvClY+lht5odlf9cvUi+p/V6566Dwth9/nq8uWxO1+8+5iumuGqLR3iFab3UDOhxB9iTUA3VqOloAtrJc10fwyEHkMz/PJ42BDaFqV4jpk42Goci2b1MI7cHp2nXNA+i8djy2LJHDdCOWkjCQm6FuaSVBLfUQES8rGY2ktUXeODeNXIHVstH2W3T19PQOpNSK1j0YgypKpug9dCSvAok7QpXwvnjG5KkALiEyeiQP8moLPfbRlrw7zRl4xddp0eAGEbgEldXQOLqzefHg9pRnldbvTx4cOzgxFd8W0Hz/ct+up3187eub6U0i33SNByzhHWoO3TNgCvA6RqCsY0Rlrksv+OT+jx5D7ROIYV/RY/TI3HYsY1rsa5faG7UbyTVajvLvKajU1xD+1dDZV4xP4BDgQBtJL+o+OjXyQ1nO1135PlPlXnmIk96VG34JF40P2pz5FQn/HLR1JpZcfLGDa3tYbH6CvX/rrw4QdvePpO0XN/jMnk/6W4Y3HyZx6V5A50Rd275mIYpPuiOYQXvwdX5A6YpR+0VDrjzY5NYJgJ8pNwOFAyszlhdVr1SSAbHyYHiNwMbpe460v3Pls6+dPXv+kDPKfuqXjB59G/6vjBva31Bgngv+RYysipurrqdP8FTUgLhLUQAAA="
}
}

