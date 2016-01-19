package scalan.linalgebra

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait MatricesAbs extends scalan.ScalanDsl with Matrices {
  self: MatricesDsl =>

  // single proxy for each type family
  implicit def proxyMatrix[T](p: Rep[Matrix[T]]): Matrix[T] = {
    proxyOps[Matrix[T]](p)(scala.reflect.classTag[Matrix[T]])
  }

  // familyElem
  class MatrixElem[T, To <: Matrix[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[Matrix[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Matrix[T]] => convertMatrix(x) }
      tryConvert(element[Matrix[T]], this, x, conv)
    }

    def convertMatrix(x: Rep[Matrix[T]]): Rep[To] = {
      x.selfType1 match {
        case _: MatrixElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have MatrixElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def matrixElement[T](implicit eT: Elem[T]): Elem[Matrix[T]] =
    cachedElem[MatrixElem[T, Matrix[T]]](eT)

  implicit case object MatrixCompanionElem extends CompanionElem[MatrixCompanionAbs] {
    lazy val tag = weakTypeTag[MatrixCompanionAbs]
    protected def getDefaultRep = Matrix
  }

  abstract class MatrixCompanionAbs extends CompanionDef[MatrixCompanionAbs] with MatrixCompanion {
    def selfType = MatrixCompanionElem
    override def toString = "Matrix"
  }
  def Matrix: Rep[MatrixCompanionAbs]
  implicit def proxyMatrixCompanionAbs(p: Rep[MatrixCompanionAbs]): MatrixCompanionAbs =
    proxyOps[MatrixCompanionAbs](p)

  abstract class AbsDenseFlatMatrix[T]
      (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T])
    extends DenseFlatMatrix[T](rmValues, numColumns) with Def[DenseFlatMatrix[T]] {
    lazy val selfType = element[DenseFlatMatrix[T]]
  }
  // elem for concrete class
  class DenseFlatMatrixElem[T](val iso: Iso[DenseFlatMatrixData[T], DenseFlatMatrix[T]])(implicit override val eT: Elem[T])
    extends MatrixElem[T, DenseFlatMatrix[T]]
    with ConcreteElem[DenseFlatMatrixData[T], DenseFlatMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(matrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertMatrix(x: Rep[Matrix[T]]) = DenseFlatMatrix(x.rmValues, x.numColumns)
    override def getDefaultRep = DenseFlatMatrix(element[Collection[T]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DenseFlatMatrix[T]]
    }
  }

  // state representation type
  type DenseFlatMatrixData[T] = (Collection[T], Int)

  // 3) Iso for concrete class
  class DenseFlatMatrixIso[T](implicit eT: Elem[T])
    extends EntityIso[DenseFlatMatrixData[T], DenseFlatMatrix[T]] with Def[DenseFlatMatrixIso[T]] {
    override def from(p: Rep[DenseFlatMatrix[T]]) =
      (p.rmValues, p.numColumns)
    override def to(p: Rep[(Collection[T], Int)]) = {
      val Pair(rmValues, numColumns) = p
      DenseFlatMatrix(rmValues, numColumns)
    }
    lazy val eFrom = pairElement(element[Collection[T]], element[Int])
    lazy val eTo = new DenseFlatMatrixElem[T](self)
    lazy val selfType = new DenseFlatMatrixIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class DenseFlatMatrixIsoElem[T](eT: Elem[T]) extends Elem[DenseFlatMatrixIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new DenseFlatMatrixIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DenseFlatMatrixIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class DenseFlatMatrixCompanionAbs extends CompanionDef[DenseFlatMatrixCompanionAbs] with DenseFlatMatrixCompanion {
    def selfType = DenseFlatMatrixCompanionElem
    override def toString = "DenseFlatMatrix"
    def apply[T](p: Rep[DenseFlatMatrixData[T]])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
      isoDenseFlatMatrix(eT).to(p)
    def apply[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
      mkDenseFlatMatrix(rmValues, numColumns)

    def unapply[T](p: Rep[Matrix[T]]) = unmkDenseFlatMatrix(p)
  }
  lazy val DenseFlatMatrixRep: Rep[DenseFlatMatrixCompanionAbs] = new DenseFlatMatrixCompanionAbs
  lazy val DenseFlatMatrix: DenseFlatMatrixCompanionAbs = proxyDenseFlatMatrixCompanion(DenseFlatMatrixRep)
  implicit def proxyDenseFlatMatrixCompanion(p: Rep[DenseFlatMatrixCompanionAbs]): DenseFlatMatrixCompanionAbs = {
    proxyOps[DenseFlatMatrixCompanionAbs](p)
  }

  implicit case object DenseFlatMatrixCompanionElem extends CompanionElem[DenseFlatMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[DenseFlatMatrixCompanionAbs]
    protected def getDefaultRep = DenseFlatMatrix
  }

  implicit def proxyDenseFlatMatrix[T](p: Rep[DenseFlatMatrix[T]]): DenseFlatMatrix[T] =
    proxyOps[DenseFlatMatrix[T]](p)

  implicit class ExtendedDenseFlatMatrix[T](p: Rep[DenseFlatMatrix[T]])(implicit eT: Elem[T]) {
    def toData: Rep[DenseFlatMatrixData[T]] = isoDenseFlatMatrix(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoDenseFlatMatrix[T](implicit eT: Elem[T]): Iso[DenseFlatMatrixData[T], DenseFlatMatrix[T]] =
    reifyObject(new DenseFlatMatrixIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkDenseFlatMatrix[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]]
  def unmkDenseFlatMatrix[T](p: Rep[Matrix[T]]): Option[(Rep[Collection[T]], Rep[Int])]

  abstract class AbsCompoundMatrix[T]
      (rows: Rep[Collection[Vector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T])
    extends CompoundMatrix[T](rows, numColumns) with Def[CompoundMatrix[T]] {
    lazy val selfType = element[CompoundMatrix[T]]
  }
  // elem for concrete class
  class CompoundMatrixElem[T](val iso: Iso[CompoundMatrixData[T], CompoundMatrix[T]])(implicit override val eT: Elem[T])
    extends MatrixElem[T, CompoundMatrix[T]]
    with ConcreteElem[CompoundMatrixData[T], CompoundMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(matrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertMatrix(x: Rep[Matrix[T]]) = CompoundMatrix(x.rows, x.numColumns)
    override def getDefaultRep = CompoundMatrix(element[Collection[Vector[T]]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[CompoundMatrix[T]]
    }
  }

  // state representation type
  type CompoundMatrixData[T] = (Collection[Vector[T]], Int)

  // 3) Iso for concrete class
  class CompoundMatrixIso[T](implicit eT: Elem[T])
    extends EntityIso[CompoundMatrixData[T], CompoundMatrix[T]] with Def[CompoundMatrixIso[T]] {
    override def from(p: Rep[CompoundMatrix[T]]) =
      (p.rows, p.numColumns)
    override def to(p: Rep[(Collection[Vector[T]], Int)]) = {
      val Pair(rows, numColumns) = p
      CompoundMatrix(rows, numColumns)
    }
    lazy val eFrom = pairElement(element[Collection[Vector[T]]], element[Int])
    lazy val eTo = new CompoundMatrixElem[T](self)
    lazy val selfType = new CompoundMatrixIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class CompoundMatrixIsoElem[T](eT: Elem[T]) extends Elem[CompoundMatrixIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new CompoundMatrixIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[CompoundMatrixIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class CompoundMatrixCompanionAbs extends CompanionDef[CompoundMatrixCompanionAbs] with CompoundMatrixCompanion {
    def selfType = CompoundMatrixCompanionElem
    override def toString = "CompoundMatrix"
    def apply[T](p: Rep[CompoundMatrixData[T]])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
      isoCompoundMatrix(eT).to(p)
    def apply[T](rows: Rep[Collection[Vector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
      mkCompoundMatrix(rows, numColumns)

    def unapply[T](p: Rep[Matrix[T]]) = unmkCompoundMatrix(p)
  }
  lazy val CompoundMatrixRep: Rep[CompoundMatrixCompanionAbs] = new CompoundMatrixCompanionAbs
  lazy val CompoundMatrix: CompoundMatrixCompanionAbs = proxyCompoundMatrixCompanion(CompoundMatrixRep)
  implicit def proxyCompoundMatrixCompanion(p: Rep[CompoundMatrixCompanionAbs]): CompoundMatrixCompanionAbs = {
    proxyOps[CompoundMatrixCompanionAbs](p)
  }

  implicit case object CompoundMatrixCompanionElem extends CompanionElem[CompoundMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[CompoundMatrixCompanionAbs]
    protected def getDefaultRep = CompoundMatrix
  }

  implicit def proxyCompoundMatrix[T](p: Rep[CompoundMatrix[T]]): CompoundMatrix[T] =
    proxyOps[CompoundMatrix[T]](p)

  implicit class ExtendedCompoundMatrix[T](p: Rep[CompoundMatrix[T]])(implicit eT: Elem[T]) {
    def toData: Rep[CompoundMatrixData[T]] = isoCompoundMatrix(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCompoundMatrix[T](implicit eT: Elem[T]): Iso[CompoundMatrixData[T], CompoundMatrix[T]] =
    reifyObject(new CompoundMatrixIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkCompoundMatrix[T](rows: Rep[Collection[Vector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]]
  def unmkCompoundMatrix[T](p: Rep[Matrix[T]]): Option[(Rep[Collection[Vector[T]]], Rep[Int])]

  abstract class AbsConstMatrix[T]
      (item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T])
    extends ConstMatrix[T](item, numColumns, numRows) with Def[ConstMatrix[T]] {
    lazy val selfType = element[ConstMatrix[T]]
  }
  // elem for concrete class
  class ConstMatrixElem[T](val iso: Iso[ConstMatrixData[T], ConstMatrix[T]])(implicit override val eT: Elem[T])
    extends MatrixElem[T, ConstMatrix[T]]
    with ConcreteElem[ConstMatrixData[T], ConstMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(matrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertMatrix(x: Rep[Matrix[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Matrix to ConstMatrix: missing fields List(item)")
    override def getDefaultRep = ConstMatrix(element[T].defaultRepValue, 0, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ConstMatrix[T]]
    }
  }

  // state representation type
  type ConstMatrixData[T] = (T, (Int, Int))

  // 3) Iso for concrete class
  class ConstMatrixIso[T](implicit eT: Elem[T])
    extends EntityIso[ConstMatrixData[T], ConstMatrix[T]] with Def[ConstMatrixIso[T]] {
    override def from(p: Rep[ConstMatrix[T]]) =
      (p.item, p.numColumns, p.numRows)
    override def to(p: Rep[(T, (Int, Int))]) = {
      val Pair(item, Pair(numColumns, numRows)) = p
      ConstMatrix(item, numColumns, numRows)
    }
    lazy val eFrom = pairElement(element[T], pairElement(element[Int], element[Int]))
    lazy val eTo = new ConstMatrixElem[T](self)
    lazy val selfType = new ConstMatrixIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class ConstMatrixIsoElem[T](eT: Elem[T]) extends Elem[ConstMatrixIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ConstMatrixIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ConstMatrixIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class ConstMatrixCompanionAbs extends CompanionDef[ConstMatrixCompanionAbs] with ConstMatrixCompanion {
    def selfType = ConstMatrixCompanionElem
    override def toString = "ConstMatrix"
    def apply[T](p: Rep[ConstMatrixData[T]])(implicit eT: Elem[T]): Rep[ConstMatrix[T]] =
      isoConstMatrix(eT).to(p)
    def apply[T](item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T]): Rep[ConstMatrix[T]] =
      mkConstMatrix(item, numColumns, numRows)

    def unapply[T](p: Rep[Matrix[T]]) = unmkConstMatrix(p)
  }
  lazy val ConstMatrixRep: Rep[ConstMatrixCompanionAbs] = new ConstMatrixCompanionAbs
  lazy val ConstMatrix: ConstMatrixCompanionAbs = proxyConstMatrixCompanion(ConstMatrixRep)
  implicit def proxyConstMatrixCompanion(p: Rep[ConstMatrixCompanionAbs]): ConstMatrixCompanionAbs = {
    proxyOps[ConstMatrixCompanionAbs](p)
  }

  implicit case object ConstMatrixCompanionElem extends CompanionElem[ConstMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[ConstMatrixCompanionAbs]
    protected def getDefaultRep = ConstMatrix
  }

  implicit def proxyConstMatrix[T](p: Rep[ConstMatrix[T]]): ConstMatrix[T] =
    proxyOps[ConstMatrix[T]](p)

  implicit class ExtendedConstMatrix[T](p: Rep[ConstMatrix[T]])(implicit eT: Elem[T]) {
    def toData: Rep[ConstMatrixData[T]] = isoConstMatrix(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConstMatrix[T](implicit eT: Elem[T]): Iso[ConstMatrixData[T], ConstMatrix[T]] =
    reifyObject(new ConstMatrixIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkConstMatrix[T](item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T]): Rep[ConstMatrix[T]]
  def unmkConstMatrix[T](p: Rep[Matrix[T]]): Option[(Rep[T], Rep[Int], Rep[Int])]

  abstract class AbsDiagonalMatrix[T]
      (diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T])
    extends DiagonalMatrix[T](diagonalValues) with Def[DiagonalMatrix[T]] {
    lazy val selfType = element[DiagonalMatrix[T]]
  }
  // elem for concrete class
  class DiagonalMatrixElem[T](val iso: Iso[DiagonalMatrixData[T], DiagonalMatrix[T]])(implicit override val eT: Elem[T])
    extends MatrixElem[T, DiagonalMatrix[T]]
    with ConcreteElem[DiagonalMatrixData[T], DiagonalMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(matrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertMatrix(x: Rep[Matrix[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Matrix to DiagonalMatrix: missing fields List(diagonalValues)")
    override def getDefaultRep = DiagonalMatrix(element[Collection[T]].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DiagonalMatrix[T]]
    }
  }

  // state representation type
  type DiagonalMatrixData[T] = Collection[T]

  // 3) Iso for concrete class
  class DiagonalMatrixIso[T](implicit eT: Elem[T])
    extends EntityIso[DiagonalMatrixData[T], DiagonalMatrix[T]] with Def[DiagonalMatrixIso[T]] {
    override def from(p: Rep[DiagonalMatrix[T]]) =
      p.diagonalValues
    override def to(p: Rep[Collection[T]]) = {
      val diagonalValues = p
      DiagonalMatrix(diagonalValues)
    }
    lazy val eFrom = element[Collection[T]]
    lazy val eTo = new DiagonalMatrixElem[T](self)
    lazy val selfType = new DiagonalMatrixIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class DiagonalMatrixIsoElem[T](eT: Elem[T]) extends Elem[DiagonalMatrixIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new DiagonalMatrixIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DiagonalMatrixIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class DiagonalMatrixCompanionAbs extends CompanionDef[DiagonalMatrixCompanionAbs] with DiagonalMatrixCompanion {
    def selfType = DiagonalMatrixCompanionElem
    override def toString = "DiagonalMatrix"

    def apply[T](diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]] =
      mkDiagonalMatrix(diagonalValues)

    def unapply[T](p: Rep[Matrix[T]]) = unmkDiagonalMatrix(p)
  }
  lazy val DiagonalMatrixRep: Rep[DiagonalMatrixCompanionAbs] = new DiagonalMatrixCompanionAbs
  lazy val DiagonalMatrix: DiagonalMatrixCompanionAbs = proxyDiagonalMatrixCompanion(DiagonalMatrixRep)
  implicit def proxyDiagonalMatrixCompanion(p: Rep[DiagonalMatrixCompanionAbs]): DiagonalMatrixCompanionAbs = {
    proxyOps[DiagonalMatrixCompanionAbs](p)
  }

  implicit case object DiagonalMatrixCompanionElem extends CompanionElem[DiagonalMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[DiagonalMatrixCompanionAbs]
    protected def getDefaultRep = DiagonalMatrix
  }

  implicit def proxyDiagonalMatrix[T](p: Rep[DiagonalMatrix[T]]): DiagonalMatrix[T] =
    proxyOps[DiagonalMatrix[T]](p)

  implicit class ExtendedDiagonalMatrix[T](p: Rep[DiagonalMatrix[T]])(implicit eT: Elem[T]) {
    def toData: Rep[DiagonalMatrixData[T]] = isoDiagonalMatrix(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoDiagonalMatrix[T](implicit eT: Elem[T]): Iso[DiagonalMatrixData[T], DiagonalMatrix[T]] =
    reifyObject(new DiagonalMatrixIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkDiagonalMatrix[T](diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]]
  def unmkDiagonalMatrix[T](p: Rep[Matrix[T]]): Option[(Rep[Collection[T]])]

  registerModule(Matrices_Module)
}

// Seq -----------------------------------
trait MatricesSeq extends scalan.ScalanDslStd with MatricesDsl {
  self: MatricesDslSeq =>
  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs {
  }

  case class SeqDenseFlatMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])(implicit eT: Elem[T])
    extends AbsDenseFlatMatrix[T](rmValues, numColumns) {
  }

  def mkDenseFlatMatrix[T]
    (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
    new SeqDenseFlatMatrix[T](rmValues, numColumns)
  def unmkDenseFlatMatrix[T](p: Rep[Matrix[T]]) = p match {
    case p: DenseFlatMatrix[T] @unchecked =>
      Some((p.rmValues, p.numColumns))
    case _ => None
  }

  case class SeqCompoundMatrix[T]
      (override val rows: Rep[Collection[Vector[T]]], override val numColumns: Rep[Int])(implicit eT: Elem[T])
    extends AbsCompoundMatrix[T](rows, numColumns) {
  }

  def mkCompoundMatrix[T]
    (rows: Rep[Collection[Vector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
    new SeqCompoundMatrix[T](rows, numColumns)
  def unmkCompoundMatrix[T](p: Rep[Matrix[T]]) = p match {
    case p: CompoundMatrix[T] @unchecked =>
      Some((p.rows, p.numColumns))
    case _ => None
  }

  case class SeqConstMatrix[T]
      (override val item: Rep[T], override val numColumns: Rep[Int], override val numRows: Rep[Int])(implicit eT: Elem[T])
    extends AbsConstMatrix[T](item, numColumns, numRows) {
  }

  def mkConstMatrix[T]
    (item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T]): Rep[ConstMatrix[T]] =
    new SeqConstMatrix[T](item, numColumns, numRows)
  def unmkConstMatrix[T](p: Rep[Matrix[T]]) = p match {
    case p: ConstMatrix[T] @unchecked =>
      Some((p.item, p.numColumns, p.numRows))
    case _ => None
  }

  case class SeqDiagonalMatrix[T]
      (override val diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T])
    extends AbsDiagonalMatrix[T](diagonalValues) {
  }

  def mkDiagonalMatrix[T]
    (diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]] =
    new SeqDiagonalMatrix[T](diagonalValues)
  def unmkDiagonalMatrix[T](p: Rep[Matrix[T]]) = p match {
    case p: DiagonalMatrix[T] @unchecked =>
      Some((p.diagonalValues))
    case _ => None
  }
}

// Exp -----------------------------------
trait MatricesExp extends scalan.ScalanDslExp with MatricesDsl {
  self: MatricesDslExp =>
  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs {
  }

  case class ExpDenseFlatMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])(implicit eT: Elem[T])
    extends AbsDenseFlatMatrix[T](rmValues, numColumns)

  object DenseFlatMatrixMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rows {
      def unapply(d: Def[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "rows" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseFlatMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseFlatMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_row {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "row" } =>
          Some((receiver, row)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromCellIndex {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iCell, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "fromCellIndex" =>
          Some((receiver, iCell)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toCellIndex {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRow, iCol, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "toCellIndex" =>
          Some((receiver, iRow, iCol)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose_block_size {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(blockSize, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "transpose" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "block_size" } =>
          Some((receiver, blockSize, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[Int], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "transpose" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceByColumns {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "reduceByColumns" =>
          Some((receiver, m, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "$times$up$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object average {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, m, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "average" =>
          Some((receiver, f, m)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DenseFlatMatrixCompanionMethods {
    object fromColumns {
      def unapply(d: Def[_]): Option[Coll[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == DenseFlatMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl {
      def unapply(d: Def[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == DenseFlatMatrixCompanionElem && method.getName == "fromNColl" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl_dense {
      def unapply(d: Def[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == DenseFlatMatrixCompanionElem && method.getName == "fromNColl" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "dense" } =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRows {
      def unapply(d: Def[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == DenseFlatMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[Vector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkDenseFlatMatrix[T]
    (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
    new ExpDenseFlatMatrix[T](rmValues, numColumns)
  def unmkDenseFlatMatrix[T](p: Rep[Matrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DenseFlatMatrixElem[T] @unchecked =>
      Some((p.asRep[DenseFlatMatrix[T]].rmValues, p.asRep[DenseFlatMatrix[T]].numColumns))
    case _ =>
      None
  }

  case class ExpCompoundMatrix[T]
      (override val rows: Rep[Collection[Vector[T]]], override val numColumns: Rep[Int])(implicit eT: Elem[T])
    extends AbsCompoundMatrix[T](rows, numColumns)

  object CompoundMatrixMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[CompoundMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[CompoundMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[CompoundMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[CompoundMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CompoundMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_row {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "row" } =>
          Some((receiver, row)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "transpose" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceByColumns {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "reduceByColumns" =>
          Some((receiver, m, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "$times$up$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object average {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, m, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "average" =>
          Some((receiver, f, m)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CompoundMatrixCompanionMethods {
    object fromColumns {
      def unapply(d: Def[_]): Option[Coll[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == CompoundMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl {
      def unapply(d: Def[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == CompoundMatrixCompanionElem && method.getName == "fromNColl" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl_dense {
      def unapply(d: Def[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == CompoundMatrixCompanionElem && method.getName == "fromNColl" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "dense" } =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRows {
      def unapply(d: Def[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == CompoundMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[Vector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkCompoundMatrix[T]
    (rows: Rep[Collection[Vector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
    new ExpCompoundMatrix[T](rows, numColumns)
  def unmkCompoundMatrix[T](p: Rep[Matrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CompoundMatrixElem[T] @unchecked =>
      Some((p.asRep[CompoundMatrix[T]].rows, p.asRep[CompoundMatrix[T]].numColumns))
    case _ =>
      None
  }

  case class ExpConstMatrix[T]
      (override val item: Rep[T], override val numColumns: Rep[Int], override val numRows: Rep[Int])(implicit eT: Elem[T])
    extends AbsConstMatrix[T](item, numColumns, numRows)

  object ConstMatrixMethods {
    object zeroValue {
      def unapply(d: Def[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "zeroValue" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object items {
      def unapply(d: Def[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rows {
      def unapply(d: Def[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "rows" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_row {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "row" } =>
          Some((receiver, row)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromCellIndex {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iCell, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "fromCellIndex" =>
          Some((receiver, iCell)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toCellIndex {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRow, iCol, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "toCellIndex" =>
          Some((receiver, iRow, iCol)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "transpose" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceByColumns {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "reduceByColumns" =>
          Some((receiver, m, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object countNonZeroesByColumns {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "countNonZeroesByColumns" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$times" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$times$up$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object average {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, m, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "average" =>
          Some((receiver, f, m)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ConstMatrixCompanionMethods {
    object fromColumns {
      def unapply(d: Def[_]): Option[Coll[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == ConstMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl {
      def unapply(d: Def[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == ConstMatrixCompanionElem && method.getName == "fromNColl" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl_dense {
      def unapply(d: Def[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == ConstMatrixCompanionElem && method.getName == "fromNColl" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "dense" } =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRows {
      def unapply(d: Def[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == ConstMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[Vector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkConstMatrix[T]
    (item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T]): Rep[ConstMatrix[T]] =
    new ExpConstMatrix[T](item, numColumns, numRows)
  def unmkConstMatrix[T](p: Rep[Matrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConstMatrixElem[T] @unchecked =>
      Some((p.asRep[ConstMatrix[T]].item, p.asRep[ConstMatrix[T]].numColumns, p.asRep[ConstMatrix[T]].numRows))
    case _ =>
      None
  }

  case class ExpDiagonalMatrix[T]
      (override val diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T])
    extends AbsDiagonalMatrix[T](diagonalValues)

  object DiagonalMatrixMethods {
    object numColumns {
      def unapply(d: Def[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "numColumns" =>
          Some(receiver).asInstanceOf[Option[Rep[DiagonalMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numRows {
      def unapply(d: Def[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "numRows" =>
          Some(receiver).asInstanceOf[Option[Rep[DiagonalMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rmValues {
      def unapply(d: Def[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[DiagonalMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object items {
      def unapply(d: Def[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[DiagonalMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "companion" =>
          Some(receiver).asInstanceOf[Option[Rep[DiagonalMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object columns {
      def unapply(d: Def[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "columns" =>
          Some(receiver).asInstanceOf[Option[Rep[DiagonalMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rows {
      def unapply(d: Def[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "rows" =>
          Some(receiver).asInstanceOf[Option[Rep[DiagonalMatrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DiagonalMatrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_row {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "row" } =>
          Some((receiver, row)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromCellIndex {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iCell, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "fromCellIndex" =>
          Some((receiver, iCell)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toCellIndex {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRow, iCol, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "toCellIndex" =>
          Some((receiver, iRow, iCol)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "transpose" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceByColumns {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "reduceByColumns" =>
          Some((receiver, m, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "$times$up$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object average {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, m, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "average" =>
          Some((receiver, f, m)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DiagonalMatrixCompanionMethods {
    object fromColumns {
      def unapply(d: Def[_]): Option[Coll[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == DiagonalMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl {
      def unapply(d: Def[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == DiagonalMatrixCompanionElem && method.getName == "fromNColl" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl_dense {
      def unapply(d: Def[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == DiagonalMatrixCompanionElem && method.getName == "fromNColl" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "dense" } =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRows {
      def unapply(d: Def[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == DiagonalMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[Vector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkDiagonalMatrix[T]
    (diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]] =
    new ExpDiagonalMatrix[T](diagonalValues)
  def unmkDiagonalMatrix[T](p: Rep[Matrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DiagonalMatrixElem[T] @unchecked =>
      Some((p.asRep[DiagonalMatrix[T]].diagonalValues))
    case _ =>
      None
  }

  object MatrixMethods {
    object numColumns {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "numColumns" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "numRows" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "rows" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "columns" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "rmValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Matrix[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Matrix[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rowsByVector {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Vec[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rowsByVector" } =>
          Some((receiver, vector)).asInstanceOf[Option[(Rep[Matrix[T]], Vec[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Vec[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_rows {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(iRows, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "rows" } =>
          Some((receiver, iRows)).asInstanceOf[Option[(Rep[Matrix[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_row {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "row" } =>
          Some((receiver, row)).asInstanceOf[Option[(Rep[Matrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[Matrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Matrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Rep[Vector[T] => Vector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object transpose {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "transpose" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[Matrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceByRows {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "reduceByRows" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[Matrix[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceByColumns {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "reduceByColumns" =>
          Some((receiver, m, n)).asInstanceOf[Option[(Rep[Matrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object countNonZeroesByColumns {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "countNonZeroesByColumns" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[Matrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "$times" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "$times$up$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[Matrix[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object average {
      def unapply(d: Def[_]): Option[(Rep[Matrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, m, _*), _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "average" =>
          Some((receiver, f, m)).asInstanceOf[Option[(Rep[Matrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Matrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[Matrix[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MatrixElem[_, _]] && method.getName == "companion" =>
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
    object fromColumns {
      def unapply(d: Def[_]): Option[Rep[Collection[Vector[T]]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == MatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Rep[Collection[Vector[T]]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[Vector[T]]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl {
      def unapply(d: Def[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == MatrixCompanionElem && method.getName == "fromNColl" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl_dense {
      def unapply(d: Def[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == MatrixCompanionElem && method.getName == "fromNColl" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "dense" } =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRows {
      def unapply(d: Def[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == MatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[Vector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[Vector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Matrices_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAM1XS2wbRRiedfx25LSlhYCICMGAQDROQKiHHKrgJKiVm0TZUCFTgcbribtldnazMw42hx57gBviikQlLki9IE4IqUJCSIgDJ4SQOHHgVIqqHugJxD+zD+8664RYJqoPo915/I/v+/7xv7fuogx30XPcwBSzeYsIPK+r52UuKvoqE6boXbJbHUpWyM60/c2ni58/8VUKTTVQ9irmK5w2UMF7WO064bNOduuogJlBuLBdLtDTdeWhatiUEkOYNqualtURuElJtW5ysVRH6abd6u2i60iroxOGzQyXCKLXKOaccH8+T2REZvheUO+9Dafvg1VlFtVIFtsuNgWEDz5OePu3iKP3mM16lkBlP7QNR4YFe3Km5diuCFzkwNxVuxW8phmGCXSqfg3v4Sq4aFd14ZqsDSdLDjbexW2yDlvk9jQEzAnd2e456n2ijoqc7AJAFyyHqpmugxACBl5WQcz38ZkP8ZmX+FR04pqYmu9jubjp2t0e8n7aBEJdB0y8dIiJwAJZZa3KB1eMtx7oJSslD3dlKDmVYRYMPTVEDYoKwPH7rY/4/ddvnkuhYgMVTb7c5MLFhohS7qNVwozZQsUcAojdNrA1N4wt5WUZ9gxIomDYloMZWPKhnASeqGmYQm6Wc5M+O0OgzwmHBFu1rqOF+c4OyVfppoYp3bzz+Nln/1h9M4VScRcFMKmD8N3AqEDZSxjk0PWNy3FKIG1bISyHQrc/5g5wHsLw/J0/W98toCupEDzf13/jC0xk+C8/l3564XwK5RtK3WsUtxuAH1+lxNpwazYTDZS394jrreT2MJVPifzlWmQHd6jwUY3CMQFwCDQ7tA4dIrFaUprXAgBKnmzXbUYqa5uVv/QfPr4lVemiSW/FK8x/zHN//1reEUqwAuVd6zKmHcIDiCegpuOgF2thJRzKhlqaDsOSwwxYYB0LjHQsluTGRc8ME45DNl3Tgotqj7z67ddv3Lu9nlHaOeVjp0L3rg0fuj6MMjttATxdYCJJMEUPFd22yMm5++bbNz8UShpaN34rbTSvQfJL6tyTB6gkuB2/uHHjzL3P3nlEVXW+aQoLO5WFI9R0UIL/Y82iOJHlmv8voZS+GF+cWiGME5Cx8Coywnm4xVupRYPMRdGW45lw1pMF0DM9YDpmYCZyNBLRtDYgrhTZDuJIyzo8VKPJKc2EapwZrkaA8NGt+ml69/ztFMpcRJkdqG5eR5mm3WGtgBv4zxWkK14L5rQ4N8AFdrEVcqF+s6if80DUamNJi+d1tOtxH4iDFZp27fdGuQKyl2HWl/RhYYx0O8jhbHJCC2p85ShaLkuFSVrGLuXH4paPS8kJCc1Ejl18OMRlCj+ZRHHtj2dkiSSfzsHprUR9j1ddJVjk478lT0fMHpeuBlN5+ERVbpm4bTNMx9m+jEEE5RU/rvFfMXHLx3bF7E/oaGqIZJ9NxHMCuqbRtTIEtQP4K8nGaA1bJu0tJvo9SJP7eUni0wPCiVkcBTU5/tbf42/MK6Oym0Qn/WaFmsBQmzRd7CfuorkhfYzut4aA+vUHn6y/+OOXv6umuiibTPh6YOEne7SZHoAwiAC+wSNBg6pk46kC/hfQyxmEExEAAA=="
}
}

