package scalan.linalgebra
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.common.Default

// Abs -----------------------------------
trait MatricesAbs extends Scalan with Matrices {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxyMatrix[T](p: Rep[Matrix[T]]): Matrix[T] =
    proxyOps[Matrix[T]](p)

  abstract class MatrixElem[T, From, To <: Matrix[T]](iso: Iso[From, To]) extends ViewElem[From, To]()(iso) {
    override def convert(x: Rep[Reifiable[_]]) = convertMatrix(x.asRep[Matrix[T]])
    def convertMatrix(x : Rep[Matrix[T]]): Rep[To]
  }

  trait MatrixCompanionElem extends CompanionElem[MatrixCompanionAbs]
  implicit lazy val MatrixCompanionElem: MatrixCompanionElem = new MatrixCompanionElem {
    lazy val tag = weakTypeTag[MatrixCompanionAbs]
    protected def getDefaultRep = Matrix
  }

  abstract class MatrixCompanionAbs extends CompanionBase[MatrixCompanionAbs] with MatrixCompanion {
    override def toString = "Matrix"
  }
  def Matrix: Rep[MatrixCompanionAbs]
  implicit def proxyMatrixCompanion(p: Rep[MatrixCompanion]): MatrixCompanion = {
    proxyOps[MatrixCompanion](p)
  }

  // elem for concrete class
  class RowMajorMatrixElem[T](iso: Iso[RowMajorMatrixData[T], RowMajorMatrix[T]])(implicit val elem: Elem[T])
    extends MatrixElem[T, RowMajorMatrixData[T], RowMajorMatrix[T]](iso) {
    def convertMatrix(x: Rep[Matrix[T]]) = RowMajorMatrix(x.rows)
  }

  // state representation type
  type RowMajorMatrixData[T] = PArray[Vector[T]]

  // 3) Iso for concrete class
  class RowMajorMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorMatrixData[T], RowMajorMatrix[T]] {
    override def from(p: Rep[RowMajorMatrix[T]]) =
      unmkRowMajorMatrix(p) match {
        case Some((rows)) => rows
        case None => !!!
      }
    override def to(p: Rep[PArray[Vector[T]]]) = {
      val rows = p
      RowMajorMatrix(rows)
    }
    lazy val tag = {
      weakTypeTag[RowMajorMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorMatrix[T]]](RowMajorMatrix(element[PArray[Vector[T]]].defaultRepValue))
    lazy val eTo = new RowMajorMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class RowMajorMatrixCompanionAbs extends CompanionBase[RowMajorMatrixCompanionAbs] with RowMajorMatrixCompanion {
    override def toString = "RowMajorMatrix"

    def apply[T](rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]): Rep[RowMajorMatrix[T]] =
      mkRowMajorMatrix(rows)
    def unapply[T:Elem](p: Rep[RowMajorMatrix[T]]) = unmkRowMajorMatrix(p)
  }
  def RowMajorMatrix: Rep[RowMajorMatrixCompanionAbs]
  implicit def proxyRowMajorMatrixCompanion(p: Rep[RowMajorMatrixCompanionAbs]): RowMajorMatrixCompanionAbs = {
    proxyOps[RowMajorMatrixCompanionAbs](p)
  }

  class RowMajorMatrixCompanionElem extends CompanionElem[RowMajorMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[RowMajorMatrixCompanionAbs]
    protected def getDefaultRep = RowMajorMatrix
  }
  implicit lazy val RowMajorMatrixCompanionElem: RowMajorMatrixCompanionElem = new RowMajorMatrixCompanionElem

  implicit def proxyRowMajorMatrix[T](p: Rep[RowMajorMatrix[T]]): RowMajorMatrix[T] =
    proxyOps[RowMajorMatrix[T]](p)

  implicit class ExtendedRowMajorMatrix[T](p: Rep[RowMajorMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorMatrixData[T]] = isoRowMajorMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorMatrix[T](implicit elem: Elem[T]): Iso[RowMajorMatrixData[T], RowMajorMatrix[T]] =
    new RowMajorMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkRowMajorMatrix[T](rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]): Rep[RowMajorMatrix[T]]
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]): Option[(Rep[PArray[Vector[T]]])]

  // elem for concrete class
  class RowMajorFlatMatrixElem[T](iso: Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]])(implicit val elem: Elem[T])
    extends MatrixElem[T, RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]](iso) {
    def convertMatrix(x: Rep[Matrix[T]]) = RowMajorFlatMatrix(x.rmValues, x.numColumns)
  }

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
      weakTypeTag[RowMajorFlatMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorFlatMatrix[T]]](RowMajorFlatMatrix(element[PArray[T]].defaultRepValue, 0))
    lazy val eTo = new RowMajorFlatMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class RowMajorFlatMatrixCompanionAbs extends CompanionBase[RowMajorFlatMatrixCompanionAbs] with RowMajorFlatMatrixCompanion {
    override def toString = "RowMajorFlatMatrix"
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
  def mkRowMajorFlatMatrix[T](rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]]
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]): Option[(Rep[PArray[T]], Rep[Int])]

  // elem for concrete class
  class RowMajorSparseMatrixElem[T](iso: Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]])(implicit val elem: Elem[T])
    extends MatrixElem[T, RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]](iso) {
    def convertMatrix(x: Rep[Matrix[T]]) = RowMajorSparseMatrix(x.rows)
  }

  // state representation type
  type RowMajorSparseMatrixData[T] = PArray[Vector[T]]

  // 3) Iso for concrete class
  class RowMajorSparseMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] {
    override def from(p: Rep[RowMajorSparseMatrix[T]]) =
      unmkRowMajorSparseMatrix(p) match {
        case Some((rows)) => rows
        case None => !!!
      }
    override def to(p: Rep[PArray[Vector[T]]]) = {
      val rows = p
      RowMajorSparseMatrix(rows)
    }
    lazy val tag = {
      weakTypeTag[RowMajorSparseMatrix[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[RowMajorSparseMatrix[T]]](RowMajorSparseMatrix(element[PArray[Vector[T]]].defaultRepValue))
    lazy val eTo = new RowMajorSparseMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class RowMajorSparseMatrixCompanionAbs extends CompanionBase[RowMajorSparseMatrixCompanionAbs] with RowMajorSparseMatrixCompanion {
    override def toString = "RowMajorSparseMatrix"

    def apply[T](rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]] =
      mkRowMajorSparseMatrix(rows)
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
  def mkRowMajorSparseMatrix[T](rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]]
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]): Option[(Rep[PArray[Vector[T]]])]
}

// Seq -----------------------------------
trait MatricesSeq extends MatricesDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs with UserTypeSeq[MatrixCompanionAbs, MatrixCompanionAbs] {
    lazy val selfType = element[MatrixCompanionAbs]
  }

  case class SeqRowMajorMatrix[T]
      (override val rows: Rep[PArray[Vector[T]]])
      (implicit elem: Elem[T])
    extends RowMajorMatrix[T](rows)
        with UserTypeSeq[Matrix[T], RowMajorMatrix[T]] {
    lazy val selfType = element[RowMajorMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }
  lazy val RowMajorMatrix = new RowMajorMatrixCompanionAbs with UserTypeSeq[RowMajorMatrixCompanionAbs, RowMajorMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorMatrixCompanionAbs]
  }

  def mkRowMajorMatrix[T]
      (rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]) =
      new SeqRowMajorMatrix[T](rows)
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]) =
    Some((p.rows))

  case class SeqRowMajorFlatMatrix[T]
      (override val rmValues: Rep[PArray[T]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
    extends RowMajorFlatMatrix[T](rmValues, numColumns)
        with UserTypeSeq[Matrix[T], RowMajorFlatMatrix[T]] {
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
      (override val rows: Rep[PArray[Vector[T]]])
      (implicit elem: Elem[T])
    extends RowMajorSparseMatrix[T](rows)
        with UserTypeSeq[Matrix[T], RowMajorSparseMatrix[T]] {
    lazy val selfType = element[RowMajorSparseMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }
  lazy val RowMajorSparseMatrix = new RowMajorSparseMatrixCompanionAbs with UserTypeSeq[RowMajorSparseMatrixCompanionAbs, RowMajorSparseMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorSparseMatrixCompanionAbs]
  }

  def mkRowMajorSparseMatrix[T]
      (rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]) =
      new SeqRowMajorSparseMatrix[T](rows)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows))
}

// Exp -----------------------------------
trait MatricesExp extends MatricesDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs with UserTypeDef[MatrixCompanionAbs, MatrixCompanionAbs] {
    lazy val selfType = element[MatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpRowMajorMatrix[T]
      (override val rows: Rep[PArray[Vector[T]]])
      (implicit elem: Elem[T])
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] && method.getName == "companion" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] && method.getName == "numRows" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] && method.getName == "numColumns" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[RowMajorMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[RowMajorMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorMatrixElem[_]] && method.getName == "rmValues" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorMatrixCompanionElem] && method.getName == "defaultOf" =>
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
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[RowMajorMatrixCompanionElem] && method.getName == "fromColumns" =>
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
    (rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]) =
    new ExpRowMajorMatrix[T](rows)
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]) =
    Some((p.rows))

  case class ExpRowMajorFlatMatrix[T]
      (override val rmValues: Rep[PArray[T]], override val numColumns: Rep[Int])
      (implicit elem: Elem[T])
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "companion" =>
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

    object rows {
      def unapply(d: Def[_]): Option[Rep[RowMajorFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixElem[_]] && method.getName == "rows" =>
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
      def unapply(d: Def[_]): Option[PA[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[RowMajorFlatMatrixCompanionElem] && method.getName == "fromColumns" =>
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
      (override val rows: Rep[PArray[Vector[T]]])
      (implicit elem: Elem[T])
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

    object numColumns {
      def unapply(d: Def[_]): Option[Rep[RowMajorSparseMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixElem[_]] && method.getName == "numColumns" =>
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
      def unapply(d: Def[_]): Option[PA[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[RowMajorSparseMatrixCompanionElem] && method.getName == "fromColumns" =>
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
    (rows: Rep[PArray[Vector[T]]])(implicit elem: Elem[T]) =
    new ExpRowMajorSparseMatrix[T](rows)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows))

  object MatrixMethods {
    object numColumns {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "numColumns" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "numRows" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "rows" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object vector_* {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "vector" } =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[Matrix[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(mat, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, mat, n)).asInstanceOf[Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _, _]] && method.getName == "companion" =>
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
    // WARNING: Cannot generate matcher for method `defaultOf`: Method's return type Default[Rep[Matrix[T]]] is not a Rep

    object fromColumns {
      def unapply(d: Def[_]): Option[PA[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem.isInstanceOf[MatrixCompanionElem] && method.getName == "fromColumns" =>
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
