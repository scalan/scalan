package scalan.linalgebra

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait MatricesAbs extends Matrices with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxyAbstractMatrix[T](p: Rep[AbstractMatrix[T]]): AbstractMatrix[T] = {
    proxyOps[AbstractMatrix[T]](p)(scala.reflect.classTag[AbstractMatrix[T]])
  }

  // familyElem
  class AbstractMatrixElem[T, To <: AbstractMatrix[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[AbstractMatrix[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[AbstractMatrix[T]] => convertAbstractMatrix(x) }
      tryConvert(element[AbstractMatrix[T]], this, x, conv)
    }

    def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]): Rep[To] = {
      x.selfType1 match {
        case _: AbstractMatrixElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have AbstractMatrixElem[_, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def abstractMatrixElement[T](implicit eT: Elem[T]): Elem[AbstractMatrix[T]] =
    cachedElem[AbstractMatrixElem[T, AbstractMatrix[T]]](eT)

  implicit case object AbstractMatrixCompanionElem extends CompanionElem[AbstractMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[AbstractMatrixCompanionAbs]
    protected def getDefaultRep = AbstractMatrix
  }

  abstract class AbstractMatrixCompanionAbs extends CompanionDef[AbstractMatrixCompanionAbs] with AbstractMatrixCompanion {
    def selfType = AbstractMatrixCompanionElem
    override def toString = "AbstractMatrix"
  }
  def AbstractMatrix: Rep[AbstractMatrixCompanionAbs]
  implicit def proxyAbstractMatrixCompanion(p: Rep[AbstractMatrixCompanion]): AbstractMatrixCompanion =
    proxyOps[AbstractMatrixCompanion](p)

  abstract class AbsDenseFlatMatrix[T]
      (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T])
    extends DenseFlatMatrix[T](rmValues, numColumns) with Def[DenseFlatMatrix[T]] {
    lazy val selfType = element[DenseFlatMatrix[T]]
  }
  // elem for concrete class
  class DenseFlatMatrixElem[T](val iso: Iso[DenseFlatMatrixData[T], DenseFlatMatrix[T]])(implicit eT: Elem[T])
    extends AbstractMatrixElem[T, DenseFlatMatrix[T]]
    with ConcreteElem[DenseFlatMatrixData[T], DenseFlatMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractMatrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = DenseFlatMatrix(x.rmValues, x.numColumns)
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
    extends Iso[DenseFlatMatrixData[T], DenseFlatMatrix[T]]()(pairElement(implicitly[Elem[Collection[T]]], implicitly[Elem[Int]])) {
    override def from(p: Rep[DenseFlatMatrix[T]]) =
      (p.rmValues, p.numColumns)
    override def to(p: Rep[(Collection[T], Int)]) = {
      val Pair(rmValues, numColumns) = p
      DenseFlatMatrix(rmValues, numColumns)
    }
    lazy val eTo = new DenseFlatMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  class DenseFlatMatrixCompanionAbs extends CompanionDef[DenseFlatMatrixCompanionAbs] with DenseFlatMatrixCompanion {
    def selfType = DenseFlatMatrixCompanionElem
    override def toString = "DenseFlatMatrix"
    def apply[T](p: Rep[DenseFlatMatrixData[T]])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
      isoDenseFlatMatrix(eT).to(p)
    def apply[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
      mkDenseFlatMatrix(rmValues, numColumns)
  }
  object DenseFlatMatrixMatcher {
    def unapply[T](p: Rep[AbstractMatrix[T]]) = unmkDenseFlatMatrix(p)
  }
  lazy val DenseFlatMatrix: Rep[DenseFlatMatrixCompanionAbs] = new DenseFlatMatrixCompanionAbs
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
    cachedIso[DenseFlatMatrixIso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkDenseFlatMatrix[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]]
  def unmkDenseFlatMatrix[T](p: Rep[AbstractMatrix[T]]): Option[(Rep[Collection[T]], Rep[Int])]

  abstract class AbsCompoundMatrix[T]
      (rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T])
    extends CompoundMatrix[T](rows, numColumns) with Def[CompoundMatrix[T]] {
    lazy val selfType = element[CompoundMatrix[T]]
  }
  // elem for concrete class
  class CompoundMatrixElem[T](val iso: Iso[CompoundMatrixData[T], CompoundMatrix[T]])(implicit eT: Elem[T])
    extends AbstractMatrixElem[T, CompoundMatrix[T]]
    with ConcreteElem[CompoundMatrixData[T], CompoundMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractMatrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = CompoundMatrix(x.rows, x.numColumns)
    override def getDefaultRep = CompoundMatrix(element[Collection[AbstractVector[T]]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[CompoundMatrix[T]]
    }
  }

  // state representation type
  type CompoundMatrixData[T] = (Collection[AbstractVector[T]], Int)

  // 3) Iso for concrete class
  class CompoundMatrixIso[T](implicit eT: Elem[T])
    extends Iso[CompoundMatrixData[T], CompoundMatrix[T]]()(pairElement(implicitly[Elem[Collection[AbstractVector[T]]]], implicitly[Elem[Int]])) {
    override def from(p: Rep[CompoundMatrix[T]]) =
      (p.rows, p.numColumns)
    override def to(p: Rep[(Collection[AbstractVector[T]], Int)]) = {
      val Pair(rows, numColumns) = p
      CompoundMatrix(rows, numColumns)
    }
    lazy val eTo = new CompoundMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  class CompoundMatrixCompanionAbs extends CompanionDef[CompoundMatrixCompanionAbs] with CompoundMatrixCompanion {
    def selfType = CompoundMatrixCompanionElem
    override def toString = "CompoundMatrix"
    def apply[T](p: Rep[CompoundMatrixData[T]])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
      isoCompoundMatrix(eT).to(p)
    def apply[T](rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
      mkCompoundMatrix(rows, numColumns)
  }
  object CompoundMatrixMatcher {
    def unapply[T](p: Rep[AbstractMatrix[T]]) = unmkCompoundMatrix(p)
  }
  lazy val CompoundMatrix: Rep[CompoundMatrixCompanionAbs] = new CompoundMatrixCompanionAbs
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
    cachedIso[CompoundMatrixIso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkCompoundMatrix[T](rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]]
  def unmkCompoundMatrix[T](p: Rep[AbstractMatrix[T]]): Option[(Rep[Collection[AbstractVector[T]]], Rep[Int])]

  abstract class AbsConstMatrix[T]
      (item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T])
    extends ConstMatrix[T](item, numColumns, numRows) with Def[ConstMatrix[T]] {
    lazy val selfType = element[ConstMatrix[T]]
  }
  // elem for concrete class
  class ConstMatrixElem[T](val iso: Iso[ConstMatrixData[T], ConstMatrix[T]])(implicit eT: Elem[T])
    extends AbstractMatrixElem[T, ConstMatrix[T]]
    with ConcreteElem[ConstMatrixData[T], ConstMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractMatrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = // Converter is not generated by meta
!!!("Cannot convert from AbstractMatrix to ConstMatrix: missing fields List(item)")
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
    extends Iso[ConstMatrixData[T], ConstMatrix[T]]()(pairElement(implicitly[Elem[T]], pairElement(implicitly[Elem[Int]], implicitly[Elem[Int]]))) {
    override def from(p: Rep[ConstMatrix[T]]) =
      (p.item, p.numColumns, p.numRows)
    override def to(p: Rep[(T, (Int, Int))]) = {
      val Pair(item, Pair(numColumns, numRows)) = p
      ConstMatrix(item, numColumns, numRows)
    }
    lazy val eTo = new ConstMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  class ConstMatrixCompanionAbs extends CompanionDef[ConstMatrixCompanionAbs] with ConstMatrixCompanion {
    def selfType = ConstMatrixCompanionElem
    override def toString = "ConstMatrix"
    def apply[T](p: Rep[ConstMatrixData[T]])(implicit eT: Elem[T]): Rep[ConstMatrix[T]] =
      isoConstMatrix(eT).to(p)
    def apply[T](item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T]): Rep[ConstMatrix[T]] =
      mkConstMatrix(item, numColumns, numRows)
  }
  object ConstMatrixMatcher {
    def unapply[T](p: Rep[AbstractMatrix[T]]) = unmkConstMatrix(p)
  }
  lazy val ConstMatrix: Rep[ConstMatrixCompanionAbs] = new ConstMatrixCompanionAbs
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
    cachedIso[ConstMatrixIso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkConstMatrix[T](item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T]): Rep[ConstMatrix[T]]
  def unmkConstMatrix[T](p: Rep[AbstractMatrix[T]]): Option[(Rep[T], Rep[Int], Rep[Int])]

  abstract class AbsDiagonalMatrix[T]
      (diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T])
    extends DiagonalMatrix[T](diagonalValues) with Def[DiagonalMatrix[T]] {
    lazy val selfType = element[DiagonalMatrix[T]]
  }
  // elem for concrete class
  class DiagonalMatrixElem[T](val iso: Iso[DiagonalMatrixData[T], DiagonalMatrix[T]])(implicit eT: Elem[T])
    extends AbstractMatrixElem[T, DiagonalMatrix[T]]
    with ConcreteElem[DiagonalMatrixData[T], DiagonalMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractMatrixElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = // Converter is not generated by meta
!!!("Cannot convert from AbstractMatrix to DiagonalMatrix: missing fields List(diagonalValues)")
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
    extends Iso[DiagonalMatrixData[T], DiagonalMatrix[T]] {
    override def from(p: Rep[DiagonalMatrix[T]]) =
      p.diagonalValues
    override def to(p: Rep[Collection[T]]) = {
      val diagonalValues = p
      DiagonalMatrix(diagonalValues)
    }
    lazy val eTo = new DiagonalMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  class DiagonalMatrixCompanionAbs extends CompanionDef[DiagonalMatrixCompanionAbs] with DiagonalMatrixCompanion {
    def selfType = DiagonalMatrixCompanionElem
    override def toString = "DiagonalMatrix"

    def apply[T](diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]] =
      mkDiagonalMatrix(diagonalValues)
  }
  object DiagonalMatrixMatcher {
    def unapply[T](p: Rep[AbstractMatrix[T]]) = unmkDiagonalMatrix(p)
  }
  lazy val DiagonalMatrix: Rep[DiagonalMatrixCompanionAbs] = new DiagonalMatrixCompanionAbs
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
    cachedIso[DiagonalMatrixIso[T]](eT)

  // 6) smart constructor and deconstructor
  def mkDiagonalMatrix[T](diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]]
  def unmkDiagonalMatrix[T](p: Rep[AbstractMatrix[T]]): Option[(Rep[Collection[T]])]

  registerModule(Matrices_Module)
}

// Seq -----------------------------------
trait MatricesSeq extends MatricesDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val AbstractMatrix: Rep[AbstractMatrixCompanionAbs] = new AbstractMatrixCompanionAbs {
  }

  case class SeqDenseFlatMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])(implicit eT: Elem[T])
    extends AbsDenseFlatMatrix[T](rmValues, numColumns) {
  }

  def mkDenseFlatMatrix[T]
    (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
    new SeqDenseFlatMatrix[T](rmValues, numColumns)
  def unmkDenseFlatMatrix[T](p: Rep[AbstractMatrix[T]]) = p match {
    case p: DenseFlatMatrix[T] @unchecked =>
      Some((p.rmValues, p.numColumns))
    case _ => None
  }

  case class SeqCompoundMatrix[T]
      (override val rows: Rep[Collection[AbstractVector[T]]], override val numColumns: Rep[Int])(implicit eT: Elem[T])
    extends AbsCompoundMatrix[T](rows, numColumns) {
  }

  def mkCompoundMatrix[T]
    (rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
    new SeqCompoundMatrix[T](rows, numColumns)
  def unmkCompoundMatrix[T](p: Rep[AbstractMatrix[T]]) = p match {
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
  def unmkConstMatrix[T](p: Rep[AbstractMatrix[T]]) = p match {
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
  def unmkDiagonalMatrix[T](p: Rep[AbstractMatrix[T]]) = p match {
    case p: DiagonalMatrix[T] @unchecked =>
      Some((p.diagonalValues))
    case _ => None
  }
}

// Exp -----------------------------------
trait MatricesExp extends MatricesDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val AbstractMatrix: Rep[AbstractMatrixCompanionAbs] = new AbstractMatrixCompanionAbs {
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
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == DenseFlatMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == DenseFlatMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkDenseFlatMatrix[T]
    (rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
    new ExpDenseFlatMatrix[T](rmValues, numColumns)
  def unmkDenseFlatMatrix[T](p: Rep[AbstractMatrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DenseFlatMatrixElem[T] @unchecked =>
      Some((p.asRep[DenseFlatMatrix[T]].rmValues, p.asRep[DenseFlatMatrix[T]].numColumns))
    case _ =>
      None
  }

  case class ExpCompoundMatrix[T]
      (override val rows: Rep[Collection[AbstractVector[T]]], override val numColumns: Rep[Int])(implicit eT: Elem[T])
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
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == CompoundMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == CompoundMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkCompoundMatrix[T]
    (rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
    new ExpCompoundMatrix[T](rows, numColumns)
  def unmkCompoundMatrix[T](p: Rep[AbstractMatrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
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
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$times" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == ConstMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == ConstMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkConstMatrix[T]
    (item: Rep[T], numColumns: Rep[Int], numRows: Rep[Int])(implicit eT: Elem[T]): Rep[ConstMatrix[T]] =
    new ExpConstMatrix[T](item, numColumns, numRows)
  def unmkConstMatrix[T](p: Rep[AbstractMatrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
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
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == DiagonalMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Coll[AbstractVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[AbstractVector[T]] forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == DiagonalMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkDiagonalMatrix[T]
    (diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]] =
    new ExpDiagonalMatrix[T](diagonalValues)
  def unmkDiagonalMatrix[T](p: Rep[AbstractMatrix[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DiagonalMatrixElem[T] @unchecked =>
      Some((p.asRep[DiagonalMatrix[T]].diagonalValues))
    case _ =>
      None
  }

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

    object apply_row {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "row" } =>
          Some((receiver, row)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(row, column, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, row, column)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]) forSome {type T; type R}] = exp match {
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

    object reduceByRows {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "reduceByRows" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceByColumns {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "reduceByColumns" =>
          Some((receiver, m, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], RepMonoid[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object countNonZeroesByColumns {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "countNonZeroesByColumns" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Vector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$times" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Vector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Vector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_* {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$times" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$plus$up$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object matrix_*^^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$times$up$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "matrix" } =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Matrix[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^^ {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "$times$up$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object average {
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, m, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "average" =>
          Some((receiver, f, m)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Fractional[T], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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
    object fromColumns {
      def unapply(d: Def[_]): Option[Rep[Collection[AbstractVector[T]]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cols, _*), _) if receiver.elem == AbstractMatrixCompanionElem && method.getName == "fromColumns" =>
          Some(cols).asInstanceOf[Option[Rep[Collection[AbstractVector[T]]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[AbstractVector[T]]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNColl {
      def unapply(d: Def[_]): Option[(NColl[(Int, T)], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == AbstractMatrixCompanionElem && method.getName == "fromNColl" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(items, numColumns, elem, _*), _) if receiver.elem == AbstractMatrixCompanionElem && method.getName == "fromNColl" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "dense" } =>
          Some((items, numColumns, elem)).asInstanceOf[Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(NColl[T], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRows {
      def unapply(d: Def[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(rows, length, _*), _) if receiver.elem == AbstractMatrixCompanionElem && method.getName == "fromRows" =>
          Some((rows, length)).asInstanceOf[Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[AbstractVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Matrices_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAM1XTW8bRRie3cRxbEdJGxWaIiJCMCAQjVMQ6iGHKjgOCnKTKBsqZCqksT1xp8zObnbGwebQHwA3xBVBj0i9cUJIFRJCQhw4IUBCHHsqRagCegLxzuyHd5110gYT1YfR7ny8H8/zvON3b9xBGeGhZ0QDM8wXbCLxgqWfl4UsWhUuqexedJptRlbIzozz5SfnPn3scxNN1dDYFSxWBKuhnP9Q6bjRs0V2qyiHeYMI6XhCoier2kOp4TBGGpI6vERtuy1xnZFSlQq5VEWjdafZ3UXXkFFFJxoOb3hEEqvMsBBEBPPjREVEo/ecfu9uuD0fvKSyKMWy2PYwlRA++Djh798irtXlDu/aEk0GoW24KizYk6W263gydJEFc1ecZvg6yjFMoOnqVbyHS+CiVbKkR3kLThZc3Hgbt8g6bFHbRyFgQdjOdtfV7yNVlBdkFwBas12mZzouQggYeFEHsdDDZyHCZ0HhU7SIRzGj72K1uOk5nS7yf8YIQh0XTLxwiInQAqnwZvG9y40371kF21SHOyqUrM5wDAw9MUANmgrA8ZutD8TdV6+fN1G+hvJULNeF9HBDxikP0Cpgzh2pY44AxF4L2JofxJb2sgx7+iSRazi2izlYCqCcAJ4YbVCpNqu5iYCdAdBnpUvCrUbHNaJ85wbkq3VTxoxt3j5z9ulfK2+YyEy6yIFJC4TvhUZBTiEaFzHIohM4UeOURMa2RloNuU5vzB4QRATHs7d/a369iC6bEYiBz/vjDUxkxE8/FL5/7oKJxmta5asMt2qAo6gwYm94ZYfLGhp39ojnr2T3MFNPqTxmm2QHt5kM0I3DMgKwSDQ3sB5dojBb0to3QgAKvnzXHU6Kq5vFv6xvP7yh1OmhCX/FL9B/6Pm/f57ckVq4Eo179iXM2kSEEI9AbSdBz5ejijiUDb00E4WlhlmwwNs2GGnbPM2Nh54aJCCXbHrUhgtrj7z81Rev/35zPaM1NB1gp0P3r48Auh6MKjtjETytcZkmmLyPiuXY5OT8XfrW9fellobRSd5OG/WrkPySPvf4ASoJb8k/a4vmH2d+/NhEORBDnUobu8XF+6zt/7FeUZK8yXLwD6HVfS65OLVCuCAg3aAKYzyHW04n67QcDzYmiWxPB8DHTJ/dxKnZ3i36SCycGaNPTSbZDoMYVYV3qCjT85mN5Dc7WH6A36Nb1VPszoWbJsq8hjI7UM6iijJ1p82bITHwZytJR74SzhlJYoAI7GE7IkL/5lAv576o9caC0cfXke7FfWD2l+ao57xzlNqPwrkEq4GmDwvnSNeDGs6mJ7aox5ceRNiTSnGKpuHq+nTS7HHJOiWb2dixtYdLaVQGSaUqbX9cR9ZJ+uksnN5KFftwJVaARTHke/NUzOZxias/j4dXWZNNilsOx2yYncwwLpuVIK4hXzZJs8d22ezP5sEkEUt9LBXMEeie/rtg4pAdwFxBdUqr2Kas209b0mm6KpOMHECjD8X+D5uj4qfGX3p7go3j2qjqL9HJoJthFLhqkbqHAxQ8ND+g0bGCxhHwv3bvo/Xnv/vslm6z86oFhe8JHn3Mx9vrJGrTvj3I3W5zKrvwkR6LHWSmulMd979ZuwZfNBEAAA=="
}
}

