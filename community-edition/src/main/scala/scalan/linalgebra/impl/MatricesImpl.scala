package scalan.linalgebra
package impl

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait MatricesAbs extends Scalan with Matrices {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxyAbstractMatrix[T](p: Rep[AbstractMatrix[T]]): AbstractMatrix[T] = {
    proxyOps[AbstractMatrix[T]](p)(classTag[AbstractMatrix[T]])
  }

  class AbstractMatrixElem[T, To <: AbstractMatrix[T]](implicit elem: Elem[T])
    extends EntityElem[To] {
    def isEntityType = true
    def tag = {
      implicit val tagT = elem.tag
      weakTypeTag[AbstractMatrix[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertAbstractMatrix(x.asRep[AbstractMatrix[T]])
    def convertAbstractMatrix(x : Rep[AbstractMatrix[T]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[AbstractMatrixElem[_,_]])
      x.asRep[To]
    }
    def getDefaultRep: Rep[To] = ???
  }

  def abstractMatrixElement[T](implicit elem: Elem[T]) =
    new AbstractMatrixElem[T, AbstractMatrix[T]]()(elem)

  trait AbstractMatrixCompanionElem extends CompanionElem[AbstractMatrixCompanionAbs]
  implicit lazy val AbstractMatrixCompanionElem: AbstractMatrixCompanionElem = new AbstractMatrixCompanionElem {
    lazy val tag = weakTypeTag[AbstractMatrixCompanionAbs]
    protected def getDefaultRep = AbstractMatrix
  }

  abstract class AbstractMatrixCompanionAbs extends CompanionBase[AbstractMatrixCompanionAbs] with AbstractMatrixCompanion {
    override def toString = "AbstractMatrix"
  }
  def AbstractMatrix: Rep[AbstractMatrixCompanionAbs]
  implicit def proxyAbstractMatrixCompanion(p: Rep[AbstractMatrixCompanion]): AbstractMatrixCompanion = {
    proxyOps[AbstractMatrixCompanion](p)
  }

  // elem for concrete class
  class RowMajorDirectMatrixElem[T](val iso: Iso[RowMajorDirectMatrixData[T], RowMajorDirectMatrix[T]])(implicit val elem: Elem[T])
    extends AbstractMatrixElem[T, RowMajorDirectMatrix[T]]
    with ViewElem[RowMajorDirectMatrixData[T], RowMajorDirectMatrix[T]] {
    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = RowMajorDirectMatrix(x.rows)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type RowMajorDirectMatrixData[T] = Collection[AbstractVector[T]]

  // 3) Iso for concrete class
  class RowMajorDirectMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorDirectMatrixData[T], RowMajorDirectMatrix[T]] {
    override def from(p: Rep[RowMajorDirectMatrix[T]]) =
      unmkRowMajorDirectMatrix(p) match {
        case Some((rows)) => rows
        case None => !!!
      }
    override def to(p: Rep[Collection[AbstractVector[T]]]) = {
      val rows = p
      RowMajorDirectMatrix(rows)
    }
    lazy val tag = {
      implicit val tagT = elem.tag
      weakTypeTag[RowMajorDirectMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorDirectMatrix[T]]](RowMajorDirectMatrix(element[Collection[AbstractVector[T]]].defaultRepValue))
    lazy val eTo = new RowMajorDirectMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class RowMajorDirectMatrixCompanionAbs extends CompanionBase[RowMajorDirectMatrixCompanionAbs] with RowMajorDirectMatrixCompanion {
    override def toString = "RowMajorDirectMatrix"

    def apply[T](rows: Rep[Collection[AbstractVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorDirectMatrix[T]] =
      mkRowMajorDirectMatrix(rows)
    def unapply[T:Elem](p: Rep[RowMajorDirectMatrix[T]]) = unmkRowMajorDirectMatrix(p)
  }
  def RowMajorDirectMatrix: Rep[RowMajorDirectMatrixCompanionAbs]
  implicit def proxyRowMajorDirectMatrixCompanion(p: Rep[RowMajorDirectMatrixCompanionAbs]): RowMajorDirectMatrixCompanionAbs = {
    proxyOps[RowMajorDirectMatrixCompanionAbs](p)
  }

  class RowMajorDirectMatrixCompanionElem extends CompanionElem[RowMajorDirectMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[RowMajorDirectMatrixCompanionAbs]
    protected def getDefaultRep = RowMajorDirectMatrix
  }
  implicit lazy val RowMajorDirectMatrixCompanionElem: RowMajorDirectMatrixCompanionElem = new RowMajorDirectMatrixCompanionElem

  implicit def proxyRowMajorDirectMatrix[T](p: Rep[RowMajorDirectMatrix[T]]): RowMajorDirectMatrix[T] =
    proxyOps[RowMajorDirectMatrix[T]](p)

  implicit class ExtendedRowMajorDirectMatrix[T](p: Rep[RowMajorDirectMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorDirectMatrixData[T]] = isoRowMajorDirectMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorDirectMatrix[T](implicit elem: Elem[T]): Iso[RowMajorDirectMatrixData[T], RowMajorDirectMatrix[T]] =
    new RowMajorDirectMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorDirectMatrix[T](rows: Rep[Collection[AbstractVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorDirectMatrix[T]]
  def unmkRowMajorDirectMatrix[T:Elem](p: Rep[RowMajorDirectMatrix[T]]): Option[(Rep[Collection[AbstractVector[T]]])]

  // elem for concrete class
  class RowMajorNestedMatrixElem[T](val iso: Iso[RowMajorNestedMatrixData[T], RowMajorNestedMatrix[T]])(implicit val elem: Elem[T])
    extends AbstractMatrixElem[T, RowMajorNestedMatrix[T]]
    with ViewElem[RowMajorNestedMatrixData[T], RowMajorNestedMatrix[T]] {
    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = RowMajorNestedMatrix(x.rmValues, x.numColumns)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type RowMajorNestedMatrixData[T] = (Collection[T], Int)

  // 3) Iso for concrete class
  class RowMajorNestedMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorNestedMatrixData[T], RowMajorNestedMatrix[T]] {
    override def from(p: Rep[RowMajorNestedMatrix[T]]) =
      unmkRowMajorNestedMatrix(p) match {
        case Some((rmValues, numColumns)) => Pair(rmValues, numColumns)
        case None => !!!
      }
    override def to(p: Rep[(Collection[T], Int)]) = {
      val Pair(rmValues, numColumns) = p
      RowMajorNestedMatrix(rmValues, numColumns)
    }
    lazy val tag = {
      implicit val tagT = elem.tag
      weakTypeTag[RowMajorNestedMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorNestedMatrix[T]]](RowMajorNestedMatrix(element[Collection[T]].defaultRepValue, 0))
    lazy val eTo = new RowMajorNestedMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class RowMajorNestedMatrixCompanionAbs extends CompanionBase[RowMajorNestedMatrixCompanionAbs] with RowMajorNestedMatrixCompanion {
    override def toString = "RowMajorNestedMatrix"
    def apply[T](p: Rep[RowMajorNestedMatrixData[T]])(implicit elem: Elem[T]): Rep[RowMajorNestedMatrix[T]] =
      isoRowMajorNestedMatrix(elem).to(p)
    def apply[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorNestedMatrix[T]] =
      mkRowMajorNestedMatrix(rmValues, numColumns)
    def unapply[T:Elem](p: Rep[RowMajorNestedMatrix[T]]) = unmkRowMajorNestedMatrix(p)
  }
  def RowMajorNestedMatrix: Rep[RowMajorNestedMatrixCompanionAbs]
  implicit def proxyRowMajorNestedMatrixCompanion(p: Rep[RowMajorNestedMatrixCompanionAbs]): RowMajorNestedMatrixCompanionAbs = {
    proxyOps[RowMajorNestedMatrixCompanionAbs](p)
  }

  class RowMajorNestedMatrixCompanionElem extends CompanionElem[RowMajorNestedMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[RowMajorNestedMatrixCompanionAbs]
    protected def getDefaultRep = RowMajorNestedMatrix
  }
  implicit lazy val RowMajorNestedMatrixCompanionElem: RowMajorNestedMatrixCompanionElem = new RowMajorNestedMatrixCompanionElem

  implicit def proxyRowMajorNestedMatrix[T](p: Rep[RowMajorNestedMatrix[T]]): RowMajorNestedMatrix[T] =
    proxyOps[RowMajorNestedMatrix[T]](p)

  implicit class ExtendedRowMajorNestedMatrix[T](p: Rep[RowMajorNestedMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorNestedMatrixData[T]] = isoRowMajorNestedMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorNestedMatrix[T](implicit elem: Elem[T]): Iso[RowMajorNestedMatrixData[T], RowMajorNestedMatrix[T]] =
    new RowMajorNestedMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorNestedMatrix[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorNestedMatrix[T]]
  def unmkRowMajorNestedMatrix[T:Elem](p: Rep[RowMajorNestedMatrix[T]]): Option[(Rep[Collection[T]], Rep[Int])]

  // elem for concrete class
  class RowMajorSparseMatrixElem[T](val iso: Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]])(implicit val elem: Elem[T])
    extends AbstractMatrixElem[T, RowMajorSparseMatrix[T]]
    with ViewElem[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] {
    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = RowMajorSparseMatrix(x.rows, x.numColumns)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type RowMajorSparseMatrixData[T] = (Collection[AbstractVector[T]], Int)

  // 3) Iso for concrete class
  class RowMajorSparseMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] {
    override def from(p: Rep[RowMajorSparseMatrix[T]]) =
      unmkRowMajorSparseMatrix(p) match {
        case Some((rows, numColumns)) => Pair(rows, numColumns)
        case None => !!!
      }
    override def to(p: Rep[(Collection[AbstractVector[T]], Int)]) = {
      val Pair(rows, numColumns) = p
      RowMajorSparseMatrix(rows, numColumns)
    }
    lazy val tag = {
      implicit val tagT = elem.tag
      weakTypeTag[RowMajorSparseMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorSparseMatrix[T]]](RowMajorSparseMatrix(element[Collection[AbstractVector[T]]].defaultRepValue, 0))
    lazy val eTo = new RowMajorSparseMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class RowMajorSparseMatrixCompanionAbs extends CompanionBase[RowMajorSparseMatrixCompanionAbs] with RowMajorSparseMatrixCompanion {
    override def toString = "RowMajorSparseMatrix"
    def apply[T](p: Rep[RowMajorSparseMatrixData[T]])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]] =
      isoRowMajorSparseMatrix(elem).to(p)
    def apply[T](rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]] =
      mkRowMajorSparseMatrix(rows, numColumns)
    def unapply[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) = unmkRowMajorSparseMatrix(p)
  }
  def RowMajorSparseMatrix: Rep[RowMajorSparseMatrixCompanionAbs]
  implicit def proxyRowMajorSparseMatrixCompanion(p: Rep[RowMajorSparseMatrixCompanionAbs]): RowMajorSparseMatrixCompanionAbs = {
    proxyOps[RowMajorSparseMatrixCompanionAbs](p)
  }

  class RowMajorSparseMatrixCompanionElem extends CompanionElem[RowMajorSparseMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[RowMajorSparseMatrixCompanionAbs]
    protected def getDefaultRep = RowMajorSparseMatrix
  }
  implicit lazy val RowMajorSparseMatrixCompanionElem: RowMajorSparseMatrixCompanionElem = new RowMajorSparseMatrixCompanionElem

  implicit def proxyRowMajorSparseMatrix[T](p: Rep[RowMajorSparseMatrix[T]]): RowMajorSparseMatrix[T] =
    proxyOps[RowMajorSparseMatrix[T]](p)

  implicit class ExtendedRowMajorSparseMatrix[T](p: Rep[RowMajorSparseMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorSparseMatrixData[T]] = isoRowMajorSparseMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorSparseMatrix[T](implicit elem: Elem[T]): Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] =
    new RowMajorSparseMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorSparseMatrix[T](rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]]
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]): Option[(Rep[Collection[AbstractVector[T]]], Rep[Int])]
}

// Seq -----------------------------------
trait MatricesSeq extends MatricesDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val AbstractMatrix: Rep[AbstractMatrixCompanionAbs] = new AbstractMatrixCompanionAbs with UserTypeSeq[AbstractMatrixCompanionAbs, AbstractMatrixCompanionAbs] {
    lazy val selfType = element[AbstractMatrixCompanionAbs]
  }

  case class SeqRowMajorDirectMatrix[T]
      (override val rows: Rep[Collection[AbstractVector[T]]])
      (implicit elem: Elem[T])
    extends RowMajorDirectMatrix[T](rows)
        with UserTypeSeq[AbstractMatrix[T], RowMajorDirectMatrix[T]] {
    lazy val selfType = element[RowMajorDirectMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
  }
  lazy val RowMajorDirectMatrix = new RowMajorDirectMatrixCompanionAbs with UserTypeSeq[RowMajorDirectMatrixCompanionAbs, RowMajorDirectMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorDirectMatrixCompanionAbs]
  }

  def mkRowMajorDirectMatrix[T]
      (rows: Rep[Collection[AbstractVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorDirectMatrix[T]] =
      new SeqRowMajorDirectMatrix[T](rows)
  def unmkRowMajorDirectMatrix[T:Elem](p: Rep[RowMajorDirectMatrix[T]]) =
    Some((p.rows))

  case class SeqRowMajorNestedMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
    extends RowMajorNestedMatrix[T](rmValues, numColumns)
        with UserTypeSeq[AbstractMatrix[T], RowMajorNestedMatrix[T]] {
    lazy val selfType = element[RowMajorNestedMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
  }
  lazy val RowMajorNestedMatrix = new RowMajorNestedMatrixCompanionAbs with UserTypeSeq[RowMajorNestedMatrixCompanionAbs, RowMajorNestedMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorNestedMatrixCompanionAbs]
  }

  def mkRowMajorNestedMatrix[T]
      (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorNestedMatrix[T]] =
      new SeqRowMajorNestedMatrix[T](rmValues, numColumns)
  def unmkRowMajorNestedMatrix[T:Elem](p: Rep[RowMajorNestedMatrix[T]]) =
    Some((p.rmValues, p.numColumns))

  case class SeqRowMajorSparseMatrix[T]
      (override val rows: Rep[Collection[AbstractVector[T]]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
    extends RowMajorSparseMatrix[T](rows, numColumns)
        with UserTypeSeq[AbstractMatrix[T], RowMajorSparseMatrix[T]] {
    lazy val selfType = element[RowMajorSparseMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
  }
  lazy val RowMajorSparseMatrix = new RowMajorSparseMatrixCompanionAbs with UserTypeSeq[RowMajorSparseMatrixCompanionAbs, RowMajorSparseMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorSparseMatrixCompanionAbs]
  }

  def mkRowMajorSparseMatrix[T]
      (rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]] =
      new SeqRowMajorSparseMatrix[T](rows, numColumns)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows, p.numColumns))
}

// Exp -----------------------------------
trait MatricesExp extends MatricesDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val AbstractMatrix: Rep[AbstractMatrixCompanionAbs] = new AbstractMatrixCompanionAbs with UserTypeDef[AbstractMatrixCompanionAbs, AbstractMatrixCompanionAbs] {
    lazy val selfType = element[AbstractMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpRowMajorDirectMatrix[T]
      (override val rows: Rep[Collection[AbstractVector[T]]])
      (implicit elem: Elem[T])
    extends RowMajorDirectMatrix[T](rows) with UserTypeDef[AbstractMatrix[T], RowMajorDirectMatrix[T]] {
    lazy val selfType = element[RowMajorDirectMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorDirectMatrix[T](t(rows))
  }

  lazy val RowMajorDirectMatrix: Rep[RowMajorDirectMatrixCompanionAbs] = new RowMajorDirectMatrixCompanionAbs with UserTypeDef[RowMajorDirectMatrixCompanionAbs, RowMajorDirectMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorDirectMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object RowMajorDirectMatrixMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numColumns {
      def unapply(d: Def[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "numColumns" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorDirectMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[RowMajorDirectMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[RowMajorDirectMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorDirectMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method's return type Vector[T] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[(Rep[RowMajorDirectMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[RowMajorDirectMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorDirectMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[RowMajorDirectMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "transpose" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[RowMajorDirectMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorDirectMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `reduceByColumns`: Method's return type Vector[T] is not a Rep

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[RowMajorDirectMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[RowMajorDirectMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorDirectMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[RowMajorDirectMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixElem[_]] && method.getName == "$times$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[RowMajorDirectMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorDirectMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `average`: Method's return type DoubleRep is not a Rep
  }

  object RowMajorDirectMatrixCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromColumns {
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[RowMajorDirectMatrixCompanionElem] && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkRowMajorDirectMatrix[T]
    (rows: Rep[Collection[AbstractVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorDirectMatrix[T]] =
    new ExpRowMajorDirectMatrix[T](rows)
  def unmkRowMajorDirectMatrix[T:Elem](p: Rep[RowMajorDirectMatrix[T]]) =
    Some((p.rows))

  case class ExpRowMajorNestedMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
    extends RowMajorNestedMatrix[T](rmValues, numColumns) with UserTypeDef[AbstractMatrix[T], RowMajorNestedMatrix[T]] {
    lazy val selfType = element[RowMajorNestedMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorNestedMatrix[T](t(rmValues), t(numColumns))
  }

  lazy val RowMajorNestedMatrix: Rep[RowMajorNestedMatrixCompanionAbs] = new RowMajorNestedMatrixCompanionAbs with UserTypeDef[RowMajorNestedMatrixCompanionAbs, RowMajorNestedMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorNestedMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object RowMajorNestedMatrixMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorNestedMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `rows`: Method's return type Coll[DenseVector[T]] is not a Rep

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method's return type Vector[T] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromCellIndex {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iCell, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "fromCellIndex" =>
          Some((receiver, iCell)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toCellIndex {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRow, iCol, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "toCellIndex" =>
          Some((receiver, iRow, iCol)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `blockCellIndices`: Method's return type Coll[Int] is not a Rep

    // WARNING: Cannot generate matcher for method `transposeIndices`: Method's return type Coll[Int] is not a Rep

    object getTranspositionOfBlocks {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Coll[((Int,Int),(Int,Int))]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(blocks, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "getTranspositionOfBlocks" =>
          Some((receiver, blocks)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Coll[((Int,Int),(Int,Int))]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Coll[((Int,Int),(Int,Int))]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose_block_size {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(blockSize, n, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "transpose" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "block_size" } =>
          Some((receiver, blockSize, n)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[Int], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "transpose"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `reduceByColumns`: Method's return type Vector[T] is not a Rep

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixElem[_]] && method.getName == "$times$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[RowMajorNestedMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorNestedMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `average`: Method's return type DoubleRep is not a Rep
  }

  object RowMajorNestedMatrixCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromColumns {
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixCompanionElem] && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRows {
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, _*), _) if receiver.elem.isInstanceOf[RowMajorNestedMatrixCompanionElem] && method.getName == "fromRows" =>
          Some(rows).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkRowMajorNestedMatrix[T]
    (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorNestedMatrix[T]] =
    new ExpRowMajorNestedMatrix[T](rmValues, numColumns)
  def unmkRowMajorNestedMatrix[T:Elem](p: Rep[RowMajorNestedMatrix[T]]) =
    Some((p.rmValues, p.numColumns))

  case class ExpRowMajorSparseMatrix[T]
      (override val rows: Rep[Collection[AbstractVector[T]]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
    extends RowMajorSparseMatrix[T](rows, numColumns) with UserTypeDef[AbstractMatrix[T], RowMajorSparseMatrix[T]] {
    lazy val selfType = element[RowMajorSparseMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorSparseMatrix[T](t(rows), t(numColumns))
  }

  lazy val RowMajorSparseMatrix: Rep[RowMajorSparseMatrixCompanionAbs] = new RowMajorSparseMatrixCompanionAbs with UserTypeDef[RowMajorSparseMatrixCompanionAbs, RowMajorSparseMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorSparseMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object RowMajorSparseMatrixMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[RowMajorSparseMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[RowMajorSparseMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorSparseMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method's return type Vector[T] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[(Rep[RowMajorSparseMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[RowMajorSparseMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorSparseMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[RowMajorSparseMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "transpose" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[RowMajorSparseMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorSparseMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `reduceByColumns`: Method's return type Vector[T] is not a Rep

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[RowMajorSparseMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[RowMajorSparseMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorSparseMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[RowMajorSparseMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "$times$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[RowMajorSparseMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorSparseMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `average`: Method's return type DoubleRep is not a Rep
  }

  object RowMajorSparseMatrixCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromColumns {
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixCompanionElem] && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkRowMajorSparseMatrix[T]
    (rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]] =
    new ExpRowMajorSparseMatrix[T](rows, numColumns)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows, p.numColumns))

  object AbstractMatrixMethods {
    object numColumns {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "numColumns" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rows {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "rows" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rowsByVector {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Vector[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rowsByVector" } =>
          Some((receiver, vector)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Vector[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Vector[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method's return type Vector[T] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "transpose" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `reduceByRows`: Method's return type Vector[T] is not a Rep

    // WARNING: Cannot generate matcher for method `reduceByColumns`: Method's return type Vector[T] is not a Rep

    // WARNING: Cannot generate matcher for method `$times`: Method's return type Vector[T] is not a Rep

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$times$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `average`: Method's return type DoubleRep is not a Rep

    object companion {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AbstractMatrixCompanionMethods {
    // WARNING: Cannot generate matcher for method `defaultOf`: Method's return type Default[Rep[AbstractMatrix[T]]] is not a Rep

    object fromColumns {
      def unapply(d: Def[_]): Option[Rep[Collection[AbstractVector[T]]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixCompanionElem] && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Rep[Collection[AbstractVector[T]]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[AbstractVector[T]]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl {
      def unapply(d: Def[_]): Option[(NColl[(Int,T)], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixCompanionElem] && method.getName == "fromNColl" =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[(Int,T)], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[(Int,T)], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
