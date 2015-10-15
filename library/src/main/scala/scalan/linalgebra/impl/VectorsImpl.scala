package scalan.linalgebra

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait VectorsAbs extends Vectors with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxyAbstractVector[T](p: Rep[AbstractVector[T]]): AbstractVector[T] = {
    proxyOps[AbstractVector[T]](p)(scala.reflect.classTag[AbstractVector[T]])
  }

  // familyElem
  class AbstractVectorElem[T, To <: AbstractVector[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Vectors")
      module.entities.find(_.name == "AbstractVector").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[AbstractVector[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[AbstractVector[T]] => convertAbstractVector(x) }
      tryConvert(element[AbstractVector[T]], this, x, conv)
    }

    def convertAbstractVector(x : Rep[AbstractVector[T]]): Rep[To] = {
      assert(x.selfType1 match { case _: AbstractVectorElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def abstractVectorElement[T](implicit eT: Elem[T]): Elem[AbstractVector[T]] =
    cachedElem[AbstractVectorElem[T, AbstractVector[T]]](eT)

  implicit case object AbstractVectorCompanionElem extends CompanionElem[AbstractVectorCompanionAbs] {
    lazy val tag = weakTypeTag[AbstractVectorCompanionAbs]
    protected def getDefaultRep = AbstractVector
  }

  abstract class AbstractVectorCompanionAbs extends CompanionBase[AbstractVectorCompanionAbs] with AbstractVectorCompanion {
    override def toString = "AbstractVector"
  }
  def AbstractVector: Rep[AbstractVectorCompanionAbs]
  implicit def proxyAbstractVectorCompanion(p: Rep[AbstractVectorCompanion]): AbstractVectorCompanion =
    proxyOps[AbstractVectorCompanion](p)

  // elem for concrete class
  class DenseVectorElem[T](val iso: Iso[DenseVectorData[T], DenseVector[T]])(implicit eT: Elem[T])
    extends AbstractVectorElem[T, DenseVector[T]]
    with ConcreteElem[DenseVectorData[T], DenseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractVectorElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Vectors")
      module.concreteSClasses.find(_.name == "DenseVector").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractVector(x: Rep[AbstractVector[T]]) = DenseVector(x.items)
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
    extends Iso[DenseVectorData[T], DenseVector[T]] {
    override def from(p: Rep[DenseVector[T]]) =
      p.items
    override def to(p: Rep[Collection[T]]) = {
      val items = p
      DenseVector(items)
    }
    lazy val eTo = new DenseVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class DenseVectorCompanionAbs extends CompanionBase[DenseVectorCompanionAbs] with DenseVectorCompanion {
    override def toString = "DenseVector"

    def apply[T](items: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
      mkDenseVector(items)
  }
  object DenseVectorMatcher {
    def unapply[T](p: Rep[AbstractVector[T]]) = unmkDenseVector(p)
  }
  def DenseVector: Rep[DenseVectorCompanionAbs]
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
    cachedIso[DenseVectorIso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkDenseVector[T](items: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]]
  def unmkDenseVector[T](p: Rep[AbstractVector[T]]): Option[(Rep[Collection[T]])]

  // elem for concrete class
  class ConstVectorElem[T](val iso: Iso[ConstVectorData[T], ConstVector[T]])(implicit eT: Elem[T])
    extends AbstractVectorElem[T, ConstVector[T]]
    with ConcreteElem[ConstVectorData[T], ConstVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractVectorElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Vectors")
      module.concreteSClasses.find(_.name == "ConstVector").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractVector(x: Rep[AbstractVector[T]]) = // Converter is not generated by meta
!!!("Cannot convert from AbstractVector to ConstVector: missing fields List(item)")
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
    extends Iso[ConstVectorData[T], ConstVector[T]]()(pairElement(implicitly[Elem[T]], implicitly[Elem[Int]])) {
    override def from(p: Rep[ConstVector[T]]) =
      (p.item, p.length)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(item, length) = p
      ConstVector(item, length)
    }
    lazy val eTo = new ConstVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class ConstVectorCompanionAbs extends CompanionBase[ConstVectorCompanionAbs] with ConstVectorCompanion {
    override def toString = "ConstVector"
    def apply[T](p: Rep[ConstVectorData[T]])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      isoConstVector(eT).to(p)
    def apply[T](item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      mkConstVector(item, length)
  }
  object ConstVectorMatcher {
    def unapply[T](p: Rep[AbstractVector[T]]) = unmkConstVector(p)
  }
  def ConstVector: Rep[ConstVectorCompanionAbs]
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
    cachedIso[ConstVectorIso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkConstVector[T](item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]]
  def unmkConstVector[T](p: Rep[AbstractVector[T]]): Option[(Rep[T], Rep[Int])]

  // elem for concrete class
  class SparseVectorElem[T](val iso: Iso[SparseVectorData[T], SparseVector[T]])(implicit eT: Elem[T])
    extends AbstractVectorElem[T, SparseVector[T]]
    with ConcreteElem[SparseVectorData[T], SparseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractVectorElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Vectors")
      module.concreteSClasses.find(_.name == "SparseVector").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractVector(x: Rep[AbstractVector[T]]) = SparseVector(x.nonZeroIndices, x.nonZeroValues, x.length)
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
    extends Iso[SparseVectorData[T], SparseVector[T]]()(pairElement(implicitly[Elem[Collection[Int]]], pairElement(implicitly[Elem[Collection[T]]], implicitly[Elem[Int]]))) {
    override def from(p: Rep[SparseVector[T]]) =
      (p.nonZeroIndices, p.nonZeroValues, p.length)
    override def to(p: Rep[(Collection[Int], (Collection[T], Int))]) = {
      val Pair(nonZeroIndices, Pair(nonZeroValues, length)) = p
      SparseVector(nonZeroIndices, nonZeroValues, length)
    }
    lazy val eTo = new SparseVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class SparseVectorCompanionAbs extends CompanionBase[SparseVectorCompanionAbs] with SparseVectorCompanion {
    override def toString = "SparseVector"
    def apply[T](p: Rep[SparseVectorData[T]])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      isoSparseVector(eT).to(p)
    def apply[T](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      mkSparseVector(nonZeroIndices, nonZeroValues, length)
  }
  object SparseVectorMatcher {
    def unapply[T](p: Rep[AbstractVector[T]]) = unmkSparseVector(p)
  }
  def SparseVector: Rep[SparseVectorCompanionAbs]
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
    cachedIso[SparseVectorIso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkSparseVector[T](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]]
  def unmkSparseVector[T](p: Rep[AbstractVector[T]]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int])]

  // elem for concrete class
  class SparseVector1Elem[T](val iso: Iso[SparseVector1Data[T], SparseVector1[T]])(implicit eT: Elem[T])
    extends AbstractVectorElem[T, SparseVector1[T]]
    with ConcreteElem[SparseVector1Data[T], SparseVector1[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractVectorElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Vectors")
      module.concreteSClasses.find(_.name == "SparseVector1").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractVector(x: Rep[AbstractVector[T]]) = SparseVector1(x.nonZeroItems, x.length)
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
    extends Iso[SparseVector1Data[T], SparseVector1[T]]()(pairElement(implicitly[Elem[Collection[(Int, T)]]], implicitly[Elem[Int]])) {
    override def from(p: Rep[SparseVector1[T]]) =
      (p.nonZeroItems, p.length)
    override def to(p: Rep[(Collection[(Int, T)], Int)]) = {
      val Pair(nonZeroItems, length) = p
      SparseVector1(nonZeroItems, length)
    }
    lazy val eTo = new SparseVector1Elem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class SparseVector1CompanionAbs extends CompanionBase[SparseVector1CompanionAbs] with SparseVector1Companion {
    override def toString = "SparseVector1"
    def apply[T](p: Rep[SparseVector1Data[T]])(implicit eT: Elem[T]): Rep[SparseVector1[T]] =
      isoSparseVector1(eT).to(p)
    def apply[T](nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector1[T]] =
      mkSparseVector1(nonZeroItems, length)
  }
  object SparseVector1Matcher {
    def unapply[T](p: Rep[AbstractVector[T]]) = unmkSparseVector1(p)
  }
  def SparseVector1: Rep[SparseVector1CompanionAbs]
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
    cachedIso[SparseVector1Iso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkSparseVector1[T](nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector1[T]]
  def unmkSparseVector1[T](p: Rep[AbstractVector[T]]): Option[(Rep[Collection[(Int, T)]], Rep[Int])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Vectors_Module.dump))
}

// Seq -----------------------------------
trait VectorsSeq extends VectorsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val AbstractVector: Rep[AbstractVectorCompanionAbs] = new AbstractVectorCompanionAbs with UserTypeSeq[AbstractVectorCompanionAbs] {
    lazy val selfType = element[AbstractVectorCompanionAbs]
  }

  case class SeqDenseVector[T]
      (override val items: Rep[Collection[T]])
      (implicit eT: Elem[T])
    extends DenseVector[T](items)
        with UserTypeSeq[DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]]
  }
  lazy val DenseVector = new DenseVectorCompanionAbs with UserTypeSeq[DenseVectorCompanionAbs] {
    lazy val selfType = element[DenseVectorCompanionAbs]
  }

  def mkDenseVector[T]
      (items: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
      new SeqDenseVector[T](items)
  def unmkDenseVector[T](p: Rep[AbstractVector[T]]) = p match {
    case p: DenseVector[T] @unchecked =>
      Some((p.items))
    case _ => None
  }

  case class SeqConstVector[T]
      (override val item: Rep[T], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends ConstVector[T](item, length)
        with UserTypeSeq[ConstVector[T]] {
    lazy val selfType = element[ConstVector[T]]
  }
  lazy val ConstVector = new ConstVectorCompanionAbs with UserTypeSeq[ConstVectorCompanionAbs] {
    lazy val selfType = element[ConstVectorCompanionAbs]
  }

  def mkConstVector[T]
      (item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      new SeqConstVector[T](item, length)
  def unmkConstVector[T](p: Rep[AbstractVector[T]]) = p match {
    case p: ConstVector[T] @unchecked =>
      Some((p.item, p.length))
    case _ => None
  }

  case class SeqSparseVector[T]
      (override val nonZeroIndices: Rep[Collection[Int]], override val nonZeroValues: Rep[Collection[T]], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length)
        with UserTypeSeq[SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]]
  }
  lazy val SparseVector = new SparseVectorCompanionAbs with UserTypeSeq[SparseVectorCompanionAbs] {
    lazy val selfType = element[SparseVectorCompanionAbs]
  }

  def mkSparseVector[T]
      (nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      new SeqSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T](p: Rep[AbstractVector[T]]) = p match {
    case p: SparseVector[T] @unchecked =>
      Some((p.nonZeroIndices, p.nonZeroValues, p.length))
    case _ => None
  }

  case class SeqSparseVector1[T]
      (override val nonZeroItems: Coll[(Int, T)], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends SparseVector1[T](nonZeroItems, length)
        with UserTypeSeq[SparseVector1[T]] {
    lazy val selfType = element[SparseVector1[T]]
  }
  lazy val SparseVector1 = new SparseVector1CompanionAbs with UserTypeSeq[SparseVector1CompanionAbs] {
    lazy val selfType = element[SparseVector1CompanionAbs]
  }

  def mkSparseVector1[T]
      (nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector1[T]] =
      new SeqSparseVector1[T](nonZeroItems, length)
  def unmkSparseVector1[T](p: Rep[AbstractVector[T]]) = p match {
    case p: SparseVector1[T] @unchecked =>
      Some((p.nonZeroItems, p.length))
    case _ => None
  }
}

// Exp -----------------------------------
trait VectorsExp extends VectorsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val AbstractVector: Rep[AbstractVectorCompanionAbs] = new AbstractVectorCompanionAbs with UserTypeDef[AbstractVectorCompanionAbs] {
    lazy val selfType = element[AbstractVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpDenseVector[T]
      (override val items: Rep[Collection[T]])
      (implicit eT: Elem[T])
    extends DenseVector[T](items) with UserTypeDef[DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]]
    override def mirror(t: Transformer) = ExpDenseVector[T](t(items))
  }

  lazy val DenseVector: Rep[DenseVectorCompanionAbs] = new DenseVectorCompanionAbs with UserTypeDef[DenseVectorCompanionAbs] {
    lazy val selfType = element[DenseVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
  def unmkDenseVector[T](p: Rep[AbstractVector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DenseVectorElem[T] @unchecked =>
      Some((p.asRep[DenseVector[T]].items))
    case _ =>
      None
  }

  case class ExpConstVector[T]
      (override val item: Rep[T], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends ConstVector[T](item, length) with UserTypeDef[ConstVector[T]] {
    lazy val selfType = element[ConstVector[T]]
    override def mirror(t: Transformer) = ExpConstVector[T](t(item), t(length))
  }

  lazy val ConstVector: Rep[ConstVectorCompanionAbs] = new ConstVectorCompanionAbs with UserTypeDef[ConstVectorCompanionAbs] {
    lazy val selfType = element[ConstVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
  def unmkConstVector[T](p: Rep[AbstractVector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConstVectorElem[T] @unchecked =>
      Some((p.asRep[ConstVector[T]].item, p.asRep[ConstVector[T]].length))
    case _ =>
      None
  }

  case class ExpSparseVector[T]
      (override val nonZeroIndices: Rep[Collection[Int]], override val nonZeroValues: Rep[Collection[T]], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length) with UserTypeDef[SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]]
    override def mirror(t: Transformer) = ExpSparseVector[T](t(nonZeroIndices), t(nonZeroValues), t(length))
  }

  lazy val SparseVector: Rep[SparseVectorCompanionAbs] = new SparseVectorCompanionAbs with UserTypeDef[SparseVectorCompanionAbs] {
    lazy val selfType = element[SparseVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[AbstractVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[AbstractVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[AbstractVector[T]], Numeric[T]) forSome {type T}] = exp match {
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
  def unmkSparseVector[T](p: Rep[AbstractVector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SparseVectorElem[T] @unchecked =>
      Some((p.asRep[SparseVector[T]].nonZeroIndices, p.asRep[SparseVector[T]].nonZeroValues, p.asRep[SparseVector[T]].length))
    case _ =>
      None
  }

  case class ExpSparseVector1[T]
      (override val nonZeroItems: Coll[(Int, T)], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends SparseVector1[T](nonZeroItems, length) with UserTypeDef[SparseVector1[T]] {
    lazy val selfType = element[SparseVector1[T]]
    override def mirror(t: Transformer) = ExpSparseVector1[T](t(nonZeroItems), t(length))
  }

  lazy val SparseVector1: Rep[SparseVector1CompanionAbs] = new SparseVector1CompanionAbs with UserTypeDef[SparseVector1CompanionAbs] {
    lazy val selfType = element[SparseVector1CompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SparseVector1[T]], Rep[AbstractVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVector1Elem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector1[T]], Rep[AbstractVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector1[T]], Rep[AbstractVector[T]], Numeric[T]) forSome {type T}] = exp match {
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
  def unmkSparseVector1[T](p: Rep[AbstractVector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SparseVector1Elem[T] @unchecked =>
      Some((p.asRep[SparseVector1[T]].nonZeroItems, p.asRep[SparseVector1[T]].length))
    case _ =>
      None
  }

  object AbstractVectorMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object items {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zeroValue {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "zeroValue" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[AbstractVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[T => R @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_collection_+^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_collection" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_value_+^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_collection_-^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_collection" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_value_-^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_collection_*^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_collection" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_value_*^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_value" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, f, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$div$up" =>
          Some((receiver, other, f)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(mat, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$times" =>
          Some((receiver, mat, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[Double], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[Double], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(num, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "euclideanNorm" =>
          Some((receiver, num)).asInstanceOf[Option[(Rep[AbstractVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[AbstractVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroesLength {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "nonZeroesLength" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AbstractVectorCompanionMethods {
    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem == AbstractVectorCompanionElem && method.getName == "zero" =>
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
        case MethodCall(receiver, method, Seq(nonZeroIndices, nonZeroValues, length, _*), _) if receiver.elem == AbstractVectorCompanionElem && method.getName == "fromSparseData" =>
          Some((nonZeroIndices, nonZeroValues, length)).asInstanceOf[Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Vectors_Module {
  val packageName = "scalan.linalgebra"
  val name = "Vectors"
  val dump = "H4sIAAAAAAAAAOVXTWwbRRSe3dhxbKdpG7WBSkSEYqioIE4RqIccSuokEOQmUdZUYKpK4/XYmTI7u9kZRzaHCnFCcENcEeq9Ny5IlXpBSIgDJwRIPfdUilAFVBxAvJn98Tq2kxRoFYk9jPbn7Xtvvu97b99ev4vSwkfPChszzGcdIvGspc8XhCxYS1xS2bng1luMLJLG+1Nf2Bf4eWGiw1U0uonFomBVlA1OltpefG6RrTLKYm4TIV1fSPRUWUco2i5jxJbU5UXqOC2Ja4wUy1TI+TJK1dx6ZwtdRUYZHbFdbvtEEqvEsBBEhPfHiMqIxtdZfd1Z87oxeFHtopjYRcXHVEL6EONIYL9BPKvDXd5xJJoIU1vzVFpgk6GO5/oyCpEBd5tuPbpMcQw30GT5Ct7GRQjRLFrSp7wJb+Y9bL+Dm2QVTJR5ChIWhDUqHU9fj5RRTpAtAGjF8Zi+0/YQQsDAizqJ2S4+szE+swqfgkV8ihl9F6uH677b7qDgMEYQanvg4vk9XEQeyBKvFz68ZL9938o7pnq5rVLJ6B2OgqMnh6hBUwE4fr3xsbj36rWzJspVUY6KhZqQPrZlkvIQrTzm3JU65xhA7DeBrZPD2NJRFsBmhySytut4mIOnEMpx4IlRm0plrO6Nh+wMgT4jPRKZGm3PiPc7M2S/WjclzNj6nRMvPPPT0psmMntDZMGlBcL3I6cgpwiNi0BCCMSoXg9LZFQ00mrJtrtrZpckYjhO3fm5/tUcumTGIIYx98cbuEiLH7/Pf/fcORONVbXKlxluVgFHscSIs+aXXC6raMzdJn7wJLONmTobyGOmThq4xWSIbhKWEYBFopmh9egRhdm81r4RAZAP5LvqclJYXi/8bn3zyXWlTh+NB0+CAv2Lnv3z1kRDauFKlKaSOCLCdwQKuxfxXCkuh31R0SUkF0S1XIccPXmPXr72kdTQG+3e6l+rXQH/8/q9J3ZhIepCv1XnzF9P/PCZibIAdo1KB3uFuX3WzkOsB9SLz0Qp7MBaPWd2POwVeQLJyOKxXotSMtcE6Jl4mQYyjy0SLsiAN6a7Dep4IpPHjUg/2kgik1SiBFJK03tSLlE+EVN7ictpehiRGrapjfIxdvfcTROlX0fpBlSJKKN0zW3xesQHfMMkacvz0T2jlw/AH/vYifHXxwzq7ndHxtowb+zKxD7bTR+QaAeQKV1WQ6uqP68+D6OM8KbcHODDR08PR3bdpw584LfJy1/eeOOXm6tp3XMnw15zEbMWCT63IYhdQFU3MOYg0gqXg3d8Sq+nD4LQIaYY9MbDFHoiZlLoan3lQIlvAiazKvHdFV6n0BIfoLmr5a1kxMEBDoUBtJz+o4/HoEhqqfS778vyIEr0uAXd6VE34/Fk0IMt0vFIpMn5I6UEs+c8Bm2u0vIYeenGH5c/eO81T48VfaNkQiv/I9lNJRVw5lHp7lBP1AcXXmLjowOhzG4Q2qDqV+zfizMJ2y7s5dWot4wdyjr7pm4YK7tRGQDS/+fzT1FU662uTWiYCQGS6GhYVYxyzJqk5uMQBB+dHFJwVjj4wvR99f6nq6e//fy2HityaoSG/w0e/+wnx4le0CYDf7B1p8Wp7MBPfCJ1kJqarnXafwPwrHw5VBEAAA=="
}
}

