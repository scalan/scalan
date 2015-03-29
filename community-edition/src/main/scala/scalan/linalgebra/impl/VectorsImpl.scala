package scalan.linalgebra
package impl

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait VectorsAbs extends Scalan with Vectors {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxyAbstractVector[T](p: Rep[AbstractVector[T]]): AbstractVector[T] = {
    implicit val tag = weakTypeTag[AbstractVector[T]]
    proxyOps[AbstractVector[T]](p)(TagImplicits.typeTagToClassTag[AbstractVector[T]])
  }

  class AbstractVectorElem[T, To <: AbstractVector[T]](implicit elem: Elem[T])
    extends EntityElem[To] {
    def isEntityType = true
    def tag = { assert(this.isInstanceOf[AbstractVectorElem[_,_]]); weakTypeTag[AbstractVector[T]].asInstanceOf[WeakTypeTag[To]]}
    override def convert(x: Rep[Reifiable[_]]) = convertAbstractVector(x.asRep[AbstractVector[T]])
    def convertAbstractVector(x : Rep[AbstractVector[T]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[AbstractVectorElem[_,_]])
      x.asRep[To]
    }
    def getDefaultRep: Rep[To] = ???
  }

  trait AbstractVectorCompanionElem extends CompanionElem[AbstractVectorCompanionAbs]
  implicit lazy val AbstractVectorCompanionElem: AbstractVectorCompanionElem = new AbstractVectorCompanionElem {
    lazy val tag = weakTypeTag[AbstractVectorCompanionAbs]
    protected def getDefaultRep = AbstractVector
  }

  abstract class AbstractVectorCompanionAbs extends CompanionBase[AbstractVectorCompanionAbs] with AbstractVectorCompanion {
    override def toString = "AbstractVector"
  }
  def AbstractVector: Rep[AbstractVectorCompanionAbs]
  implicit def proxyAbstractVectorCompanion(p: Rep[AbstractVectorCompanion]): AbstractVectorCompanion = {
    proxyOps[AbstractVectorCompanion](p)
  }

  // elem for concrete class
  class DenseVectorElem[T](val iso: Iso[DenseVectorData[T], DenseVector[T]])(implicit val elem: Elem[T])
    extends AbstractVectorElem[T, DenseVector[T]]
    with ViewElem[DenseVectorData[T], DenseVector[T]] {
    override def convertAbstractVector(x: Rep[AbstractVector[T]]) = DenseVector(x.items)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type DenseVectorData[T] = Collection[T]

  // 3) Iso for concrete class
  class DenseVectorIso[T](implicit elem: Elem[T])
    extends Iso[DenseVectorData[T], DenseVector[T]] {
    override def from(p: Rep[DenseVector[T]]) =
      unmkDenseVector(p) match {
        case Some((items)) => items
        case None => !!!
      }
    override def to(p: Rep[Collection[T]]) = {
      val items = p
      DenseVector(items)
    }
    lazy val tag = {
      weakTypeTag[DenseVector[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[DenseVector[T]]](DenseVector(element[Collection[T]].defaultRepValue))
    lazy val eTo = new DenseVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class DenseVectorCompanionAbs extends CompanionBase[DenseVectorCompanionAbs] with DenseVectorCompanion {
    override def toString = "DenseVector"

    def apply[T](items: Rep[Collection[T]])(implicit elem: Elem[T]): Rep[DenseVector[T]] =
      mkDenseVector(items)
    def unapply[T:Elem](p: Rep[DenseVector[T]]) = unmkDenseVector(p)
  }
  def DenseVector: Rep[DenseVectorCompanionAbs]
  implicit def proxyDenseVectorCompanion(p: Rep[DenseVectorCompanionAbs]): DenseVectorCompanionAbs = {
    proxyOps[DenseVectorCompanionAbs](p)
  }

  class DenseVectorCompanionElem extends CompanionElem[DenseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[DenseVectorCompanionAbs]
    protected def getDefaultRep = DenseVector
  }
  implicit lazy val DenseVectorCompanionElem: DenseVectorCompanionElem = new DenseVectorCompanionElem

  implicit def proxyDenseVector[T](p: Rep[DenseVector[T]]): DenseVector[T] =
    proxyOps[DenseVector[T]](p)

  implicit class ExtendedDenseVector[T](p: Rep[DenseVector[T]])(implicit elem: Elem[T]) {
    def toData: Rep[DenseVectorData[T]] = isoDenseVector(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoDenseVector[T](implicit elem: Elem[T]): Iso[DenseVectorData[T], DenseVector[T]] =
    new DenseVectorIso[T]

  // 6) smart constructor and deconstructor
  def mkDenseVector[T](items: Rep[Collection[T]])(implicit elem: Elem[T]): Rep[DenseVector[T]]
  def unmkDenseVector[T:Elem](p: Rep[DenseVector[T]]): Option[(Rep[Collection[T]])]

  // elem for concrete class
  class SparseVectorElem[T](val iso: Iso[SparseVectorData[T], SparseVector[T]])(implicit val elem: Elem[T])
    extends AbstractVectorElem[T, SparseVector[T]]
    with ViewElem[SparseVectorData[T], SparseVector[T]] {
    override def convertAbstractVector(x: Rep[AbstractVector[T]]) = SparseVector(x.nonZeroIndices, x.nonZeroValues, x.length)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type SparseVectorData[T] = (Collection[Int], (Collection[T], Int))

  // 3) Iso for concrete class
  class SparseVectorIso[T](implicit elem: Elem[T])
    extends Iso[SparseVectorData[T], SparseVector[T]] {
    override def from(p: Rep[SparseVector[T]]) =
      unmkSparseVector(p) match {
        case Some((nonZeroIndices, nonZeroValues, length)) => Pair(nonZeroIndices, Pair(nonZeroValues, length))
        case None => !!!
      }
    override def to(p: Rep[(Collection[Int], (Collection[T], Int))]) = {
      val Pair(nonZeroIndices, Pair(nonZeroValues, length)) = p
      SparseVector(nonZeroIndices, nonZeroValues, length)
    }
    lazy val tag = {
      weakTypeTag[SparseVector[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SparseVector[T]]](SparseVector(element[Collection[Int]].defaultRepValue, element[Collection[T]].defaultRepValue, 0))
    lazy val eTo = new SparseVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class SparseVectorCompanionAbs extends CompanionBase[SparseVectorCompanionAbs] with SparseVectorCompanion {
    override def toString = "SparseVector"
    def apply[T](p: Rep[SparseVectorData[T]])(implicit elem: Elem[T]): Rep[SparseVector[T]] =
      isoSparseVector(elem).to(p)
    def apply[T](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit elem: Elem[T]): Rep[SparseVector[T]] =
      mkSparseVector(nonZeroIndices, nonZeroValues, length)
    def unapply[T:Elem](p: Rep[SparseVector[T]]) = unmkSparseVector(p)
  }
  def SparseVector: Rep[SparseVectorCompanionAbs]
  implicit def proxySparseVectorCompanion(p: Rep[SparseVectorCompanionAbs]): SparseVectorCompanionAbs = {
    proxyOps[SparseVectorCompanionAbs](p)
  }

  class SparseVectorCompanionElem extends CompanionElem[SparseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[SparseVectorCompanionAbs]
    protected def getDefaultRep = SparseVector
  }
  implicit lazy val SparseVectorCompanionElem: SparseVectorCompanionElem = new SparseVectorCompanionElem

  implicit def proxySparseVector[T](p: Rep[SparseVector[T]]): SparseVector[T] =
    proxyOps[SparseVector[T]](p)

  implicit class ExtendedSparseVector[T](p: Rep[SparseVector[T]])(implicit elem: Elem[T]) {
    def toData: Rep[SparseVectorData[T]] = isoSparseVector(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSparseVector[T](implicit elem: Elem[T]): Iso[SparseVectorData[T], SparseVector[T]] =
    new SparseVectorIso[T]

  // 6) smart constructor and deconstructor
  def mkSparseVector[T](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit elem: Elem[T]): Rep[SparseVector[T]]
  def unmkSparseVector[T:Elem](p: Rep[SparseVector[T]]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int])]
}

// Seq -----------------------------------
trait VectorsSeq extends VectorsDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val AbstractVector: Rep[AbstractVectorCompanionAbs] = new AbstractVectorCompanionAbs with UserTypeSeq[AbstractVectorCompanionAbs, AbstractVectorCompanionAbs] {
    lazy val selfType = element[AbstractVectorCompanionAbs]
  }

  case class SeqDenseVector[T]
      (override val items: Rep[Collection[T]])
      (implicit elem: Elem[T])
    extends DenseVector[T](items)
        with UserTypeSeq[AbstractVector[T], DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]].asInstanceOf[Elem[AbstractVector[T]]]
  }
  lazy val DenseVector = new DenseVectorCompanionAbs with UserTypeSeq[DenseVectorCompanionAbs, DenseVectorCompanionAbs] {
    lazy val selfType = element[DenseVectorCompanionAbs]
  }

  def mkDenseVector[T]
      (items: Rep[Collection[T]])(implicit elem: Elem[T]): Rep[DenseVector[T]] =
      new SeqDenseVector[T](items)
  def unmkDenseVector[T:Elem](p: Rep[DenseVector[T]]) =
    Some((p.items))

  case class SeqSparseVector[T]
      (override val nonZeroIndices: Rep[Collection[Int]], override val nonZeroValues: Rep[Collection[T]], override val length: Rep[Int])
      (implicit elem: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length)
        with UserTypeSeq[AbstractVector[T], SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]].asInstanceOf[Elem[AbstractVector[T]]]
  }
  lazy val SparseVector = new SparseVectorCompanionAbs with UserTypeSeq[SparseVectorCompanionAbs, SparseVectorCompanionAbs] {
    lazy val selfType = element[SparseVectorCompanionAbs]
  }

  def mkSparseVector[T]
      (nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit elem: Elem[T]): Rep[SparseVector[T]] =
      new SeqSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T:Elem](p: Rep[SparseVector[T]]) =
    Some((p.nonZeroIndices, p.nonZeroValues, p.length))
}

// Exp -----------------------------------
trait VectorsExp extends VectorsDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val AbstractVector: Rep[AbstractVectorCompanionAbs] = new AbstractVectorCompanionAbs with UserTypeDef[AbstractVectorCompanionAbs, AbstractVectorCompanionAbs] {
    lazy val selfType = element[AbstractVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpDenseVector[T]
      (override val items: Rep[Collection[T]])
      (implicit elem: Elem[T])
    extends DenseVector[T](items) with UserTypeDef[AbstractVector[T], DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]].asInstanceOf[Elem[AbstractVector[T]]]
    override def mirror(t: Transformer) = ExpDenseVector[T](t(items))
  }

  lazy val DenseVector: Rep[DenseVectorCompanionAbs] = new DenseVectorCompanionAbs with UserTypeDef[DenseVectorCompanionAbs, DenseVectorCompanionAbs] {
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
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$plus$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$minus$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_collection_-^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_collection" } =>
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
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$times$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
  }

  object DenseVectorCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem.isInstanceOf[DenseVectorCompanionElem] && method.getName == "zero" =>
          Some(len).asInstanceOf[Option[Rep[Int] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkDenseVector[T]
    (items: Rep[Collection[T]])(implicit elem: Elem[T]): Rep[DenseVector[T]] =
    new ExpDenseVector[T](items)
  def unmkDenseVector[T:Elem](p: Rep[DenseVector[T]]) =
    Some((p.items))

  case class ExpSparseVector[T]
      (override val nonZeroIndices: Rep[Collection[Int]], override val nonZeroValues: Rep[Collection[T]], override val length: Rep[Int])
      (implicit elem: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length) with UserTypeDef[AbstractVector[T], SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]].asInstanceOf[Elem[AbstractVector[T]]]
    override def mirror(t: Transformer) = ExpSparseVector[T](t(nonZeroIndices), t(nonZeroValues), t(length))
  }

  lazy val SparseVector: Rep[SparseVectorCompanionAbs] = new SparseVectorCompanionAbs with UserTypeDef[SparseVectorCompanionAbs, SparseVectorCompanionAbs] {
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
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$plus$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$minus$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_collection_-^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_collection" } =>
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
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$times$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
  }

  object SparseVectorCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorCompanionElem] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, n)).asInstanceOf[Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_SparseVectorCompanion_apply_nonZeroItems {
      def unapply(d: Def[_]): Option[(Rep[IPairCollection[Int,T]], Rep[Int], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(nonZeroItems, length, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorCompanionElem] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "SparseVectorCompanion_apply_nonZeroItems" } =>
          Some((nonZeroItems, length, n)).asInstanceOf[Option[(Rep[IPairCollection[Int,T]], Rep[Int], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IPairCollection[Int,T]], Rep[Int], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem.isInstanceOf[SparseVectorCompanionElem] && method.getName == "zero" =>
          Some(len).asInstanceOf[Option[Rep[Int] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkSparseVector[T]
    (nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])(implicit elem: Elem[T]): Rep[SparseVector[T]] =
    new ExpSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T:Elem](p: Rep[SparseVector[T]]) =
    Some((p.nonZeroIndices, p.nonZeroValues, p.length))

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
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[AbstractVector[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractVector[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractVector[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$plus$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$minus$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractVectorElem[_, _]] && method.getName == "$times$up"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
  }

  object AbstractVectorCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractVectorCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zero {
      def unapply(d: Def[_]): Option[Rep[Int] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, _*), _) if receiver.elem.isInstanceOf[AbstractVectorCompanionElem] && method.getName == "zero" =>
          Some(len).asInstanceOf[Option[Rep[Int] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
