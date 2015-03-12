package scalan.linalgebra
package impl

import scalan._
import scalan.common.OverloadHack.Overloaded1
import scalan.common.Default
import scalan.linalgebra.Math
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait MatricesAbs extends Scalan with Matrices {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxyAbstractMatrix[T](p: Rep[AbstractMatrix[T]]): AbstractMatrix[T] = {
    implicit val tag = weakTypeTag[AbstractMatrix[T]]
    proxyOps[AbstractMatrix[T]](p)(TagImplicits.typeTagToClassTag[AbstractMatrix[T]])
  }

  abstract class AbstractMatrixElem[T, From, To <: AbstractMatrix[T]](iso: Iso[From, To])
    extends ViewElem[From, To](iso) {
    override def convert(x: Rep[Reifiable[_]]) = convertAbstractMatrix(x.asRep[AbstractMatrix[T]])
    def convertAbstractMatrix(x : Rep[AbstractMatrix[T]]): Rep[To]
  }

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
  class RowMajorFlatMatrixElem[T](iso: Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]])(implicit val elem: Elem[T])
    extends AbstractMatrixElem[T, RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]](iso) {
    def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = RowMajorFlatMatrix(x.rmValues, x.numColumns)
  }

  // state representation type
  type RowMajorFlatMatrixData[T] = (Collection[T], Int)

  // 3) Iso for concrete class
  class RowMajorFlatMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]] {
    override def from(p: Rep[RowMajorFlatMatrix[T]]) =
      unmkRowMajorFlatMatrix(p) match {
        case Some((rmValues, numColumns)) => Pair(rmValues, numColumns)
        case None => !!!
      }
    override def to(p: Rep[(Collection[T], Int)]) = {
      val Pair(rmValues, numColumns) = p
      RowMajorFlatMatrix(rmValues, numColumns)
    }
    lazy val tag = {
      weakTypeTag[RowMajorFlatMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorFlatMatrix[T]]](RowMajorFlatMatrix(element[Collection[T]].defaultRepValue, 0))
    lazy val eTo = new RowMajorFlatMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class RowMajorFlatMatrixCompanionAbs extends CompanionBase[RowMajorFlatMatrixCompanionAbs] with RowMajorFlatMatrixCompanion {
    override def toString = "RowMajorFlatMatrix"
    def apply[T](p: Rep[RowMajorFlatMatrixData[T]])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
      isoRowMajorFlatMatrix(elem).to(p)
    def apply[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
      mkRowMajorFlatMatrix(rmValues, numColumns)
    def unapply[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) = unmkRowMajorFlatMatrix(p)
  }
  def RowMajorFlatMatrix: Rep[RowMajorFlatMatrixCompanionAbs]
  implicit def proxyRowMajorFlatMatrixCompanion(p: Rep[RowMajorFlatMatrixCompanionAbs]): RowMajorFlatMatrixCompanionAbs = {
    proxyOps[RowMajorFlatMatrixCompanionAbs](p)
  }

  class RowMajorFlatMatrixCompanionElem extends CompanionElem[RowMajorFlatMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[RowMajorFlatMatrixCompanionAbs]
    protected def getDefaultRep = RowMajorFlatMatrix
  }
  implicit lazy val RowMajorFlatMatrixCompanionElem: RowMajorFlatMatrixCompanionElem = new RowMajorFlatMatrixCompanionElem

  implicit def proxyRowMajorFlatMatrix[T](p: Rep[RowMajorFlatMatrix[T]]): RowMajorFlatMatrix[T] =
    proxyOps[RowMajorFlatMatrix[T]](p)

  implicit class ExtendedRowMajorFlatMatrix[T](p: Rep[RowMajorFlatMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorFlatMatrixData[T]] = isoRowMajorFlatMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorFlatMatrix[T](implicit elem: Elem[T]): Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]] =
    new RowMajorFlatMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorFlatMatrix[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]]
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]): Option[(Rep[Collection[T]], Rep[Int])]

  // elem for concrete class
  class RowMajorSparseMatrixElem[T](iso: Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]])(implicit val elem: Elem[T])
    extends AbstractMatrixElem[T, RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]](iso) {
    def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = RowMajorSparseMatrix(x.rows, x.numColumns)
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

  case class SeqRowMajorFlatMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
    extends RowMajorFlatMatrix[T](rmValues, numColumns)
        with UserTypeSeq[AbstractMatrix[T], RowMajorFlatMatrix[T]] {
    lazy val selfType = element[RowMajorFlatMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
  }
  lazy val RowMajorFlatMatrix = new RowMajorFlatMatrixCompanionAbs with UserTypeSeq[RowMajorFlatMatrixCompanionAbs, RowMajorFlatMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorFlatMatrixCompanionAbs]
  }

  def mkRowMajorFlatMatrix[T]
      (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
      new SeqRowMajorFlatMatrix[T](rmValues, numColumns)
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) =
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

  case class ExpRowMajorFlatMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
    extends RowMajorFlatMatrix[T](rmValues, numColumns) with UserTypeDef[AbstractMatrix[T], RowMajorFlatMatrix[T]] {
    lazy val selfType = element[RowMajorFlatMatrix[T]].asInstanceOf[Elem[AbstractMatrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorFlatMatrix[T](t(rmValues), t(numColumns))
  }

  lazy val RowMajorFlatMatrix: Rep[RowMajorFlatMatrixCompanionAbs] = new RowMajorFlatMatrixCompanionAbs with UserTypeDef[RowMajorFlatMatrixCompanionAbs, RowMajorFlatMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorFlatMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object RowMajorFlatMatrixMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `rows`: Method's return type Coll[DenseVector[T]] is not a Rep

    // WARNING: Cannot generate matcher for method `apply`: Method's return type Vector[T] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromCellIndex {
      def unapply(d: Def[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iCell, _*), _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "fromCellIndex" =>
          Some((receiver, iCell)).asInstanceOf[Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toCellIndex {
      def unapply(d: Def[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRow, iCol, _*), _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "toCellIndex" =>
          Some((receiver, iRow, iCol)).asInstanceOf[Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `blockCellIndices`: Method's return type Coll[Int] is not a Rep

    // WARNING: Cannot generate matcher for method `transposeIndices`: Method's return type Coll[Int] is not a Rep

    // WARNING: Cannot generate matcher for method `getTranspositionOfBlocks`: Method's return type PairColl[Int,Int] is not a Rep

    object transpose_block_size {
      def unapply(d: Def[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(blockSize, _*), _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "transpose" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "block_size" } =>
          Some((receiver, blockSize)).asInstanceOf[Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[RowMajorFlatMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "transpose"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object RowMajorFlatMatrixCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixCompanionElem] && method.getName == "defaultOf" =>
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
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixCompanionElem] && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkRowMajorFlatMatrix[T]
    (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
    new ExpRowMajorFlatMatrix[T](rmValues, numColumns)
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) =
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
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "transpose" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "numColumns" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "numRows" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "rows" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "columns" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method's return type Vector[T] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "transpose" =>
          Some(receiver).asInstanceOf[Option[Rep[AbstractMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `$times`: Method's return type Vector[T] is not a Rep

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(mat, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, mat, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[AbstractMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _, _]] && method.getName == "companion" =>
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

    object apply {
      def unapply(d: Def[_]): Option[(Coll[A], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixCompanionElem] && method.getName == "apply" =>
          Some((items, numColumns)).asInstanceOf[Option[(Coll[A], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[A], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
