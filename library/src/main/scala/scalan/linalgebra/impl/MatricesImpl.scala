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
  class AbstractMatrixElem[T, To <: AbstractMatrix[T]](implicit val eT: Elem[T])
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Matrices")
      module.entities.find(_.name == "AbstractMatrix").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[AbstractMatrix[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[AbstractMatrix[T]] => convertAbstractMatrix(x) }
      tryConvert(element[AbstractMatrix[T]], this, x, conv)
    }

    def convertAbstractMatrix(x : Rep[AbstractMatrix[T]]): Rep[To] = {
      assert(x.selfType1 match { case _: AbstractMatrixElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def abstractMatrixElement[T](implicit eT: Elem[T]): Elem[AbstractMatrix[T]] =
    new AbstractMatrixElem[T, AbstractMatrix[T]]

  implicit case object AbstractMatrixCompanionElem extends CompanionElem[AbstractMatrixCompanionAbs] {
    lazy val tag = weakTypeTag[AbstractMatrixCompanionAbs]
    protected def getDefaultRep = AbstractMatrix
  }

  abstract class AbstractMatrixCompanionAbs extends CompanionBase[AbstractMatrixCompanionAbs] with AbstractMatrixCompanion {
    override def toString = "AbstractMatrix"
  }
  def AbstractMatrix: Rep[AbstractMatrixCompanionAbs]
  implicit def proxyAbstractMatrixCompanion(p: Rep[AbstractMatrixCompanion]): AbstractMatrixCompanion =
    proxyOps[AbstractMatrixCompanion](p)

  // elem for concrete class
  class DenseFlatMatrixElem[T](val iso: Iso[DenseFlatMatrixData[T], DenseFlatMatrix[T]])(implicit eT: Elem[T])
    extends AbstractMatrixElem[T, DenseFlatMatrix[T]]
    with ConcreteElem[DenseFlatMatrixData[T], DenseFlatMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractMatrixElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Matrices")
      module.concreteSClasses.find(_.name == "DenseFlatMatrix").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = DenseFlatMatrix(x.rmValues, x.numColumns)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
    lazy val defaultRepTo: Rep[DenseFlatMatrix[T]] = DenseFlatMatrix(element[Collection[T]].defaultRepValue, 0)
    lazy val eTo = new DenseFlatMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class DenseFlatMatrixCompanionAbs extends CompanionBase[DenseFlatMatrixCompanionAbs] with DenseFlatMatrixCompanion {
    override def toString = "DenseFlatMatrix"
    def apply[T](p: Rep[DenseFlatMatrixData[T]])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
      isoDenseFlatMatrix(eT).to(p)
    def apply[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]] =
      mkDenseFlatMatrix(rmValues, numColumns)
  }
  object DenseFlatMatrixMatcher {
    def unapply[T](p: Rep[AbstractMatrix[T]]) = unmkDenseFlatMatrix(p)
  }
  def DenseFlatMatrix: Rep[DenseFlatMatrixCompanionAbs]
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
    new DenseFlatMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkDenseFlatMatrix[T](rmValues: Rep[Collection[T]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[DenseFlatMatrix[T]]
  def unmkDenseFlatMatrix[T](p: Rep[AbstractMatrix[T]]): Option[(Rep[Collection[T]], Rep[Int])]

  // elem for concrete class
  class CompoundMatrixElem[T](val iso: Iso[CompoundMatrixData[T], CompoundMatrix[T]])(implicit eT: Elem[T])
    extends AbstractMatrixElem[T, CompoundMatrix[T]]
    with ConcreteElem[CompoundMatrixData[T], CompoundMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractMatrixElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Matrices")
      module.concreteSClasses.find(_.name == "CompoundMatrix").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = CompoundMatrix(x.rows, x.numColumns)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
    lazy val defaultRepTo: Rep[CompoundMatrix[T]] = CompoundMatrix(element[Collection[AbstractVector[T]]].defaultRepValue, 0)
    lazy val eTo = new CompoundMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class CompoundMatrixCompanionAbs extends CompanionBase[CompoundMatrixCompanionAbs] with CompoundMatrixCompanion {
    override def toString = "CompoundMatrix"
    def apply[T](p: Rep[CompoundMatrixData[T]])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
      isoCompoundMatrix(eT).to(p)
    def apply[T](rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
      mkCompoundMatrix(rows, numColumns)
  }
  object CompoundMatrixMatcher {
    def unapply[T](p: Rep[AbstractMatrix[T]]) = unmkCompoundMatrix(p)
  }
  def CompoundMatrix: Rep[CompoundMatrixCompanionAbs]
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
    new CompoundMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkCompoundMatrix[T](rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]]
  def unmkCompoundMatrix[T](p: Rep[AbstractMatrix[T]]): Option[(Rep[Collection[AbstractVector[T]]], Rep[Int])]

  // elem for concrete class
  class DiagonalMatrixElem[T](val iso: Iso[DiagonalMatrixData[T], DiagonalMatrix[T]])(implicit eT: Elem[T])
    extends AbstractMatrixElem[T, DiagonalMatrix[T]]
    with ConcreteElem[DiagonalMatrixData[T], DiagonalMatrix[T]] {
    override lazy val parent: Option[Elem[_]] = Some(abstractMatrixElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Matrices")
      module.concreteSClasses.find(_.name == "DiagonalMatrix").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertAbstractMatrix(x: Rep[AbstractMatrix[T]]) = // Converter is not generated by meta
!!!("Cannot convert from AbstractMatrix to DiagonalMatrix: missing fields List(diagonalValues)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
    lazy val defaultRepTo: Rep[DiagonalMatrix[T]] = DiagonalMatrix(element[Collection[T]].defaultRepValue)
    lazy val eTo = new DiagonalMatrixElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class DiagonalMatrixCompanionAbs extends CompanionBase[DiagonalMatrixCompanionAbs] with DiagonalMatrixCompanion {
    override def toString = "DiagonalMatrix"

    def apply[T](diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]] =
      mkDiagonalMatrix(diagonalValues)
  }
  object DiagonalMatrixMatcher {
    def unapply[T](p: Rep[AbstractMatrix[T]]) = unmkDiagonalMatrix(p)
  }
  def DiagonalMatrix: Rep[DiagonalMatrixCompanionAbs]
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
    new DiagonalMatrixIso[T]

  // 6) smart constructor and deconstructor
  def mkDiagonalMatrix[T](diagonalValues: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[DiagonalMatrix[T]]
  def unmkDiagonalMatrix[T](p: Rep[AbstractMatrix[T]]): Option[(Rep[Collection[T]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Matrices_Module.dump))
}

// Seq -----------------------------------
trait MatricesSeq extends MatricesDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val AbstractMatrix: Rep[AbstractMatrixCompanionAbs] = new AbstractMatrixCompanionAbs with UserTypeSeq[AbstractMatrixCompanionAbs] {
    lazy val selfType = element[AbstractMatrixCompanionAbs]
  }

  case class SeqDenseFlatMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])
      (implicit eT: Elem[T])
    extends DenseFlatMatrix[T](rmValues, numColumns)
        with UserTypeSeq[DenseFlatMatrix[T]] {
    lazy val selfType = element[DenseFlatMatrix[T]]
  }
  lazy val DenseFlatMatrix = new DenseFlatMatrixCompanionAbs with UserTypeSeq[DenseFlatMatrixCompanionAbs] {
    lazy val selfType = element[DenseFlatMatrixCompanionAbs]
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
      (override val rows: Rep[Collection[AbstractVector[T]]], override val numColumns: Rep[Int])
      (implicit eT: Elem[T])
    extends CompoundMatrix[T](rows, numColumns)
        with UserTypeSeq[CompoundMatrix[T]] {
    lazy val selfType = element[CompoundMatrix[T]]
  }
  lazy val CompoundMatrix = new CompoundMatrixCompanionAbs with UserTypeSeq[CompoundMatrixCompanionAbs] {
    lazy val selfType = element[CompoundMatrixCompanionAbs]
  }

  def mkCompoundMatrix[T]
      (rows: Rep[Collection[AbstractVector[T]]], numColumns: Rep[Int])(implicit eT: Elem[T]): Rep[CompoundMatrix[T]] =
      new SeqCompoundMatrix[T](rows, numColumns)
  def unmkCompoundMatrix[T](p: Rep[AbstractMatrix[T]]) = p match {
    case p: CompoundMatrix[T] @unchecked =>
      Some((p.rows, p.numColumns))
    case _ => None
  }

  case class SeqDiagonalMatrix[T]
      (override val diagonalValues: Rep[Collection[T]])
      (implicit eT: Elem[T])
    extends DiagonalMatrix[T](diagonalValues)
        with UserTypeSeq[DiagonalMatrix[T]] {
    lazy val selfType = element[DiagonalMatrix[T]]
  }
  lazy val DiagonalMatrix = new DiagonalMatrixCompanionAbs with UserTypeSeq[DiagonalMatrixCompanionAbs] {
    lazy val selfType = element[DiagonalMatrixCompanionAbs]
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
  lazy val AbstractMatrix: Rep[AbstractMatrixCompanionAbs] = new AbstractMatrixCompanionAbs with UserTypeDef[AbstractMatrixCompanionAbs] {
    lazy val selfType = element[AbstractMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpDenseFlatMatrix[T]
      (override val rmValues: Rep[Collection[T]], override val numColumns: Rep[Int])
      (implicit eT: Elem[T])
    extends DenseFlatMatrix[T](rmValues, numColumns) with UserTypeDef[DenseFlatMatrix[T]] {
    lazy val selfType = element[DenseFlatMatrix[T]]
    override def mirror(t: Transformer) = ExpDenseFlatMatrix[T](t(rmValues), t(numColumns))
  }

  lazy val DenseFlatMatrix: Rep[DenseFlatMatrixCompanionAbs] = new DenseFlatMatrixCompanionAbs with UserTypeDef[DenseFlatMatrixCompanionAbs] {
    lazy val selfType = element[DenseFlatMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      def unapply(d: Def[_]): Option[(Rep[DenseFlatMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[DenseFlatMatrixElem[_]] && method.getName == "columns" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[DenseFlatMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseFlatMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
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
      (override val rows: Rep[Collection[AbstractVector[T]]], override val numColumns: Rep[Int])
      (implicit eT: Elem[T])
    extends CompoundMatrix[T](rows, numColumns) with UserTypeDef[CompoundMatrix[T]] {
    lazy val selfType = element[CompoundMatrix[T]]
    override def mirror(t: Transformer) = ExpCompoundMatrix[T](t(rows), t(numColumns))
  }

  lazy val CompoundMatrix: Rep[CompoundMatrixCompanionAbs] = new CompoundMatrixCompanionAbs with UserTypeDef[CompoundMatrixCompanionAbs] {
    lazy val selfType = element[CompoundMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      def unapply(d: Def[_]): Option[(Rep[CompoundMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[CompoundMatrixElem[_]] && method.getName == "columns" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[CompoundMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CompoundMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
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

  case class ExpDiagonalMatrix[T]
      (override val diagonalValues: Rep[Collection[T]])
      (implicit eT: Elem[T])
    extends DiagonalMatrix[T](diagonalValues) with UserTypeDef[DiagonalMatrix[T]] {
    lazy val selfType = element[DiagonalMatrix[T]]
    override def mirror(t: Transformer) = ExpDiagonalMatrix[T](t(diagonalValues))
  }

  lazy val DiagonalMatrix: Rep[DiagonalMatrixCompanionAbs] = new DiagonalMatrixCompanionAbs with UserTypeDef[DiagonalMatrixCompanionAbs] {
    lazy val selfType = element[DiagonalMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      def unapply(d: Def[_]): Option[(Rep[DiagonalMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[DiagonalMatrixElem[_]] && method.getName == "columns" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[DiagonalMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DiagonalMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[AbstractMatrixElem[_, _]] && method.getName == "columns" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbstractMatrix[T]], Numeric[T]) forSome {type T}] = exp match {
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

object Matrices_Module {
  val packageName = "scalan.linalgebra"
  val name = "Matrices"
  val dump = "H4sIAAAAAAAAAM1XS2wbRRie3djxK0raCNpUIiIEAwLROIBQDzlUqZOgIOehbKiQqZDG67E7ZXZ2szMONoceOMINcUWoZ3rjgoTUC0JCHDghQOLMqbRCFdATiH9mH9517KQNoWIPo92df//H933/zOzNOygrfPSssDHDfMEhEi9Y+n5ZyLK1yiWVvQ232WFkhbTeP/OFvcEvCRNN1dH4VSxWBKujQnCz2vXie4vs1VABc5sI6fpCoqdqOkLFdhkjtqQur1DH6UjcYKRSo0Iu1VCm4TZ7e+g6MmrolO1y2yeSWFWGhSAifJ8nKiMaPxf0c2/L68fgFVVFJVHFro+phPQhxqnAfod4Vo+7vOdINBmmtuWptMAmRx3P9WUUIgfurrrN6DHDMbxA07VreB9XIES7Ykmf8jZ8WfKw/Q5uk00wUeYZSFgQ1trtefp5rIaKguwBQOuOx/SbrocQAgZe1kks9PFZiPFZUPiULeJTzOh7WE1u+263h4LLGEOo64GLF49wEXkgq7xZ/uCK/dZ9q+SY6uOuSiWnKxwHR0+OUIOmAnD8Zucjce+1GxdMVKyjIhXLDSF9bMsk5SFaJcy5K3XOMYDYbwNb86PY0lGWwWZAEgXbdTzMwVMI5QTwxKhNpTJW7yZCdkZAn5MeiUyNrmfE9c6NqFfrpooZ27597vwzv66+aSIzHaIALi0Qvh85BTlFaGxgkEU3DKLGKYmMXY20Ggrd/pg7JIkYjudu321+vYiumDGIYcwH4w1cZMVPP5S+f/6iifJ1rfI1htt1wFGsMuJs+VWXyzrKu/vED2Zy+5ipu6E85pqkhTtMhugmYRkDWCSaG9mPHlGYLWntGxEApUC+my4n5bXt8p/Wtx/fVOr00UQwEzTo3/TCXz9PtqQWrkR537mMWYeICOIx6O006MVq3BFHsqGnZuK01DALHnjHAScdhw8L46OnRwnII9s+dWDB2ievfvXlG7/d2sxqDU2H2OnUg+UjhK4Po6rOWIRI61wOE0wxQMVyHXJ6/h59+8aHUkvD6KZXp63GNSh+SX/3xCEqiVbJP+qL5u/nfvzURAUQQ4NKB3vlxQfs7f+wX1GavMlquENodb+UnpxaIVwQkG7YhQmeI5Oz6T6tJpNNSCLX1wHwMTPgN/XVbH8VfTyRzowxoCaT7EZJZFTjHSnK4fXMxvKbHS0/wO/MTu0xdufiLRNlX0fZFrSzqKFsw+3wZkQMbLaSdOWl6J2RJgaIwD52YiL0NYf6NQ9krQ1LxgBfx1oXD4A52JoZ3333OL0fp3MZZkNNH5XOsZYHNZwfXtiiHl95GGFPKsUpmk5W12fTbh+VrIdUM5v4bP1/pbTJJsVtl2N2kvvNSUhiJczrhCWRdvvIJHGwmoeTRKL08aFgFnYIbVF1Kv73skkCdwh/JbWrrWGHst4geemgw7WZ5uUQMgNADh5Cj4uiGj/r24SGee1UnQXQ6XDnYRQYa5OGj0MUfDQ/YlOywk0eThrX73+y+cJ3n/+ij0RFdVyAsx+Pf7ySR6E0atOBP6jd6XD4nYMfqkTuIDZ1ktB5/wOQkxSw4A4AAA=="
}
}

