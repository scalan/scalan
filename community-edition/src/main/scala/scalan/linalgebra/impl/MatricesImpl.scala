package scalan.linalgebra
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.common.Default

trait MatricesAbs extends Matrices
{ self: MatricesDsl =>
  // single proxy for each type family
  implicit def proxyMatrix[T:Elem](p: Rep[Matrix[T]]): Matrix[T] =
    proxyOps[Matrix[T]](p)

  abstract class MatrixElem[From,To](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait MatrixCompanionElem extends CompanionElem[MatrixCompanionAbs]
  implicit lazy val MatrixCompanionElem: MatrixCompanionElem = new MatrixCompanionElem {
    lazy val tag = typeTag[MatrixCompanionAbs]
    lazy val defaultRep = Default.defaultVal(Matrix)
  }

  trait MatrixCompanionAbs extends MatrixCompanion
  def Matrix: Rep[MatrixCompanionAbs]
  implicit def proxyMatrixCompanion(p: Rep[MatrixCompanion]): MatrixCompanion = {
    proxyOps[MatrixCompanion](p)
  }

  // elem for concrete class
  class RowMajorMatrixElem[T](iso: Iso[RowMajorMatrixData[T], RowMajorMatrix[T]]) extends MatrixElem[RowMajorMatrixData[T], RowMajorMatrix[T]](iso)

  // state representation type
  type RowMajorMatrixData[T] = PArray[DenseVector[T]]

  // 3) Iso for concrete class
  class RowMajorMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorMatrixData[T], RowMajorMatrix[T]] {
    override def from(p: Rep[RowMajorMatrix[T]]) =
      unmkRowMajorMatrix(p) match {
        case Some((rows)) => rows
        case None => !!!
      }
    override def to(p: Rep[PArray[DenseVector[T]]]) = {
      val rows = p
      RowMajorMatrix(rows)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[RowMajorMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorMatrix[T]]](RowMajorMatrix(element[PArray[DenseVector[T]]].defaultRepValue))
    lazy val eTo = new RowMajorMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  trait RowMajorMatrixCompanionAbs extends RowMajorMatrixCompanion {

    def apply[T](rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorMatrix[T]] =
      mkRowMajorMatrix(rows)
    def unapply[T:Elem](p: Rep[RowMajorMatrix[T]]) = unmkRowMajorMatrix(p)
  }
  def RowMajorMatrix: Rep[RowMajorMatrixCompanionAbs]
  implicit def proxyRowMajorMatrixCompanion(p: Rep[RowMajorMatrixCompanionAbs]): RowMajorMatrixCompanionAbs = {
    proxyOps[RowMajorMatrixCompanionAbs](p)
  }

  class RowMajorMatrixCompanionElem extends CompanionElem[RowMajorMatrixCompanionAbs] {
    lazy val tag = typeTag[RowMajorMatrixCompanionAbs]
    lazy val defaultRep = Default.defaultVal(RowMajorMatrix)
  }
  implicit lazy val RowMajorMatrixCompanionElem: RowMajorMatrixCompanionElem = new RowMajorMatrixCompanionElem

  implicit def proxyRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]): RowMajorMatrix[T] = {
    proxyOps[RowMajorMatrix[T]](p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorMatrix[T](implicit elem: Elem[T]): Iso[RowMajorMatrixData[T], RowMajorMatrix[T]] =
    new RowMajorMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorMatrix[T](rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorMatrix[T]]
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]): Option[(Rep[PArray[DenseVector[T]]])]

  // elem for concrete class
  class RowMajorFlatMatrixElem[T](iso: Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]]) extends MatrixElem[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]](iso)

  // state representation type
  type RowMajorFlatMatrixData[T] = (PArray[T], Int)

  // 3) Iso for concrete class
  class RowMajorFlatMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]] {
    override def from(p: Rep[RowMajorFlatMatrix[T]]) =
      unmkRowMajorFlatMatrix(p) match {
        case Some((rmValues, numColumns)) => Pair(rmValues, numColumns)
        case None => !!!
      }
    override def to(p: Rep[(PArray[T], Int)]) = {
      val Pair(rmValues, numColumns) = p
      RowMajorFlatMatrix(rmValues, numColumns)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[RowMajorFlatMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorFlatMatrix[T]]](RowMajorFlatMatrix(element[PArray[T]].defaultRepValue, 0))
    lazy val eTo = new RowMajorFlatMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  trait RowMajorFlatMatrixCompanionAbs extends RowMajorFlatMatrixCompanion {
    def apply[T](p: Rep[RowMajorFlatMatrixData[T]])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
      isoRowMajorFlatMatrix(elem).to(p)
    def apply[T](rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
      mkRowMajorFlatMatrix(rmValues, numColumns)
    def unapply[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) = unmkRowMajorFlatMatrix(p)
  }
  def RowMajorFlatMatrix: Rep[RowMajorFlatMatrixCompanionAbs]
  implicit def proxyRowMajorFlatMatrixCompanion(p: Rep[RowMajorFlatMatrixCompanionAbs]): RowMajorFlatMatrixCompanionAbs = {
    proxyOps[RowMajorFlatMatrixCompanionAbs](p)
  }

  class RowMajorFlatMatrixCompanionElem extends CompanionElem[RowMajorFlatMatrixCompanionAbs] {
    lazy val tag = typeTag[RowMajorFlatMatrixCompanionAbs]
    lazy val defaultRep = Default.defaultVal(RowMajorFlatMatrix)
  }
  implicit lazy val RowMajorFlatMatrixCompanionElem: RowMajorFlatMatrixCompanionElem = new RowMajorFlatMatrixCompanionElem

  implicit def proxyRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]): RowMajorFlatMatrix[T] = {
    proxyOps[RowMajorFlatMatrix[T]](p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorFlatMatrix[T](implicit elem: Elem[T]): Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]] =
    new RowMajorFlatMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorFlatMatrix[T](rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]]
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]): Option[(Rep[PArray[T]], Rep[Int])]

  // elem for concrete class
  class RowMajorSparseMatrixElem[T](iso: Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]]) extends MatrixElem[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]](iso)

  // state representation type
  type RowMajorSparseMatrixData[T] = PArray[SparseVector[T]]

  // 3) Iso for concrete class
  class RowMajorSparseMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] {
    override def from(p: Rep[RowMajorSparseMatrix[T]]) =
      unmkRowMajorSparseMatrix(p) match {
        case Some((rows)) => rows
        case None => !!!
      }
    override def to(p: Rep[PArray[SparseVector[T]]]) = {
      val rows = p
      RowMajorSparseMatrix(rows)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[RowMajorSparseMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorSparseMatrix[T]]](RowMajorSparseMatrix(element[PArray[SparseVector[T]]].defaultRepValue))
    lazy val eTo = new RowMajorSparseMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  trait RowMajorSparseMatrixCompanionAbs extends RowMajorSparseMatrixCompanion {

    def apply[T](rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]] =
      mkRowMajorSparseMatrix(rows)
    def unapply[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) = unmkRowMajorSparseMatrix(p)
  }
  def RowMajorSparseMatrix: Rep[RowMajorSparseMatrixCompanionAbs]
  implicit def proxyRowMajorSparseMatrixCompanion(p: Rep[RowMajorSparseMatrixCompanionAbs]): RowMajorSparseMatrixCompanionAbs = {
    proxyOps[RowMajorSparseMatrixCompanionAbs](p)
  }

  class RowMajorSparseMatrixCompanionElem extends CompanionElem[RowMajorSparseMatrixCompanionAbs] {
    lazy val tag = typeTag[RowMajorSparseMatrixCompanionAbs]
    lazy val defaultRep = Default.defaultVal(RowMajorSparseMatrix)
  }
  implicit lazy val RowMajorSparseMatrixCompanionElem: RowMajorSparseMatrixCompanionElem = new RowMajorSparseMatrixCompanionElem

  implicit def proxyRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]): RowMajorSparseMatrix[T] = {
    proxyOps[RowMajorSparseMatrix[T]](p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorSparseMatrix[T](implicit elem: Elem[T]): Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] =
    new RowMajorSparseMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorSparseMatrix[T](rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]]
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]): Option[(Rep[PArray[SparseVector[T]]])]
}

trait MatricesSeq extends MatricesAbs { self: ScalanSeq with MatricesDsl =>
  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs with UserTypeSeq[MatrixCompanionAbs, MatrixCompanionAbs] {
    lazy val selfType = element[MatrixCompanionAbs]
  }

  case class SeqRowMajorMatrix[T]
      (override val rows: Rep[PArray[DenseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorMatrix[T](rows) with UserTypeSeq[Matrix[T], RowMajorMatrix[T]] {
    lazy val selfType = element[RowMajorMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }
  lazy val RowMajorMatrix = new RowMajorMatrixCompanionAbs with UserTypeSeq[RowMajorMatrixCompanionAbs, RowMajorMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorMatrixCompanionAbs]
  }

  def mkRowMajorMatrix[T]
      (rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]) =
      new SeqRowMajorMatrix[T](rows)
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]) =
    Some((p.rows))

  case class SeqRowMajorFlatMatrix[T]
      (override val rmValues: Rep[PArray[T]], override val numColumns: Rep[Int])
      (implicit override val elem: Elem[T])
    extends RowMajorFlatMatrix[T](rmValues, numColumns) with UserTypeSeq[Matrix[T], RowMajorFlatMatrix[T]] {
    lazy val selfType = element[RowMajorFlatMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }
  lazy val RowMajorFlatMatrix = new RowMajorFlatMatrixCompanionAbs with UserTypeSeq[RowMajorFlatMatrixCompanionAbs, RowMajorFlatMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorFlatMatrixCompanionAbs]
  }

  def mkRowMajorFlatMatrix[T]
      (rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]) =
      new SeqRowMajorFlatMatrix[T](rmValues, numColumns)
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) =
    Some((p.rmValues, p.numColumns))

  case class SeqRowMajorSparseMatrix[T]
      (override val rows: Rep[PArray[SparseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorSparseMatrix[T](rows) with UserTypeSeq[Matrix[T], RowMajorSparseMatrix[T]] {
    lazy val selfType = element[RowMajorSparseMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }
  lazy val RowMajorSparseMatrix = new RowMajorSparseMatrixCompanionAbs with UserTypeSeq[RowMajorSparseMatrixCompanionAbs, RowMajorSparseMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorSparseMatrixCompanionAbs]
  }

  def mkRowMajorSparseMatrix[T]
      (rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]) =
      new SeqRowMajorSparseMatrix[T](rows)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows))
}

trait MatricesExp extends MatricesAbs { self: ScalanExp with MatricesDsl =>
  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs with UserTypeDef[MatrixCompanionAbs, MatrixCompanionAbs] {
    lazy val selfType = element[MatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpRowMajorMatrix[T]
      (override val rows: Rep[PArray[DenseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorMatrix[T](rows) with UserTypeDef[Matrix[T], RowMajorMatrix[T]] {
    lazy val selfType = element[RowMajorMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorMatrix[T](t(rows))
  }

  lazy val RowMajorMatrix: Rep[RowMajorMatrixCompanionAbs] = new RowMajorMatrixCompanionAbs with UserTypeDef[RowMajorMatrixCompanionAbs, RowMajorMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object RowMajorMatrixMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "numRows" && receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numColumns {
      def unapply(d: Def[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "numColumns" && receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "columns" && receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object RowMajorMatrixCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[RowMajorMatrixCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromColumns {
      def unapply(d: Def[_]): Option[PA[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*)) if method.getName == "fromColumns" && receiver.elem.isInstanceOf[RowMajorMatrixCompanionElem] =>
          Some(cols).asInstanceOf[Option[PA[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[PA[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkRowMajorMatrix[T]
    (rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]) =
    new ExpRowMajorMatrix[T](rows)
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]) =
    Some((p.rows))

  case class ExpRowMajorFlatMatrix[T]
      (override val rmValues: Rep[PArray[T]], override val numColumns: Rep[Int])
      (implicit override val elem: Elem[T])
    extends RowMajorFlatMatrix[T](rmValues, numColumns) with UserTypeDef[Matrix[T], RowMajorFlatMatrix[T]] {
    lazy val selfType = element[RowMajorFlatMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorFlatMatrix[T](t(rmValues), t(numColumns))
  }

  lazy val RowMajorFlatMatrix: Rep[RowMajorFlatMatrixCompanionAbs] = new RowMajorFlatMatrixCompanionAbs with UserTypeDef[RowMajorFlatMatrixCompanionAbs, RowMajorFlatMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorFlatMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object RowMajorFlatMatrixMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] =>
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
        case MethodCall(receiver, method, _) if method.getName == "numRows" && receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] =>
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
        case MethodCall(receiver, method, _) if method.getName == "columns" && receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rows {
      def unapply(d: Def[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "rows" && receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] =>
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
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[RowMajorFlatMatrixCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromColumns {
      def unapply(d: Def[_]): Option[PA[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*)) if method.getName == "fromColumns" && receiver.elem.isInstanceOf[RowMajorFlatMatrixCompanionElem] =>
          Some(cols).asInstanceOf[Option[PA[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[PA[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkRowMajorFlatMatrix[T]
    (rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]) =
    new ExpRowMajorFlatMatrix[T](rmValues, numColumns)
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) =
    Some((p.rmValues, p.numColumns))

  case class ExpRowMajorSparseMatrix[T]
      (override val rows: Rep[PArray[SparseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorSparseMatrix[T](rows) with UserTypeDef[Matrix[T], RowMajorSparseMatrix[T]] {
    lazy val selfType = element[RowMajorSparseMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorSparseMatrix[T](t(rows))
  }

  lazy val RowMajorSparseMatrix: Rep[RowMajorSparseMatrixCompanionAbs] = new RowMajorSparseMatrixCompanionAbs with UserTypeDef[RowMajorSparseMatrixCompanionAbs, RowMajorSparseMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorSparseMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object RowMajorSparseMatrixMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] =>
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
        case MethodCall(receiver, method, _) if method.getName == "columns" && receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] =>
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
        case MethodCall(receiver, method, _) if method.getName == "numRows" && receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numColumns {
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "numColumns" && receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] =>
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
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[RowMajorSparseMatrixCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromColumns {
      def unapply(d: Def[_]): Option[PA[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*)) if method.getName == "fromColumns" && receiver.elem.isInstanceOf[RowMajorSparseMatrixCompanionElem] =>
          Some(cols).asInstanceOf[Option[PA[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[PA[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkRowMajorSparseMatrix[T]
    (rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]) =
    new ExpRowMajorSparseMatrix[T](rows)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows))

  object MatrixMethods {
    object numColumns {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "numColumns" && receiver.elem.isInstanceOf[MatrixElem[_, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "numRows" && receiver.elem.isInstanceOf[MatrixElem[_, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rows {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "rows" && receiver.elem.isInstanceOf[MatrixElem[_, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "columns" && receiver.elem.isInstanceOf[MatrixElem[_, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Vec[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, _*)) if method.getName == "$times" && receiver.elem.isInstanceOf[MatrixElem[_, _]] =>
          Some((receiver, vector)).asInstanceOf[Option[(Rep[Matrix[T]], Vec[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Vec[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *! {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Matr[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(mat, _*)) if method.getName == "$times" && receiver.elem.isInstanceOf[MatrixElem[_, _]] =>
          Some((receiver, mat)).asInstanceOf[Option[(Rep[Matrix[T]], Matr[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Matr[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[MatrixElem[_, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MatrixCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[MatrixCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromColumns {
      def unapply(d: Def[_]): Option[PA[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*)) if method.getName == "fromColumns" && receiver.elem.isInstanceOf[MatrixCompanionElem] =>
          Some(cols).asInstanceOf[Option[PA[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[PA[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
