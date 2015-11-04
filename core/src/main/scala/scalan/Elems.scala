package scalan

import java.lang.reflect.Method

import scalan.common.Default
import Default._
import scalan.common.Lazy
import scalan.staged.BaseExp
import annotation.implicitNotFound
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

trait Elems extends Base { self: Scalan =>


  type Elem[A] = Element[A] // typeclass witnessing that type A can be an element of other data type (i.e. belongs to Family)
  type LElem[A] = Lazy[Elem[A]] // lazy element

  @implicitNotFound(msg = "No Element available for ${A}.")
  abstract class Element[A] extends Serializable { _: scala.Equals =>
    def isEntityType: Boolean
    def isBaseType: Boolean = this.isInstanceOf[BaseElem[_]]
    def tag: WeakTypeTag[A]
    final def classTag: ClassTag[A] = TagImplicits.typeTagToClassTag(tag)
    final def runtimeClass: Class[_] = classTag.runtimeClass
    // should only be called by defaultRepValue
    protected def getDefaultRep: Rep[A]
    lazy val defaultRepValue = getDefaultRep
    protected def getName = cleanUpTypeName(tag.tpe)
    lazy val name = getName

    override def toString = s"${getClass.getSimpleName}{$name}"

    def <:<(e: Element[_]) = tag.tpe <:< e.tag.tpe
    def >:>(e: Element[_]) = e <:< this

    def asElem[B]: Elem[B] = this.asInstanceOf[Elem[B]]

    if (isDebug) {
      debug$ElementCounter(this) += 1
    }
  }

  private val debug$ElementCounter = counter[Elem[_]]

  protected val elemCache = collection.mutable.Map.empty[(Class[_], Seq[AnyRef]), AnyRef]

  def cachedElem[E <: Elem[_]](args: AnyRef*)(implicit tag: ClassTag[E]) = {
    val clazz = tag.runtimeClass
    elemCache.getOrElseUpdate(
      (clazz, args), {
        val constructors = clazz.getDeclaredConstructors()
        if (constructors.length != 1) {
          !!!(s"Element class $clazz has ${constructors.length} constructors, 1 expected")
        } else {
          val constructor = constructors(0)
          val constructorArgs = self +: args
          constructor.newInstance(constructorArgs: _*).asInstanceOf[Elem[_]]
        }
      }).asInstanceOf[E]
  }

  def cleanUpTypeName(tpe: Type) = tpe.toString.
    replaceAll("[A-Za-z0-9_.]*this.", "").
    replace("scala.math.Numeric$", "").
    replace("scala.math.Ordering$", "").
    replace("scala.", "").
    replace("java.lang.", "").
    replaceAll("""[^# \[\],>]*[#$]""", "")

  def element[A](implicit ea: Elem[A]): Elem[A] = ea

  class BaseElem[A](implicit val tag: WeakTypeTag[A], z: Default[A]) extends Element[A] with Serializable with scala.Equals {
    protected def getDefaultRep = toRep(z.value)(this)
    override def isEntityType = false
    override def canEqual(other: Any) = other.isInstanceOf[BaseElem[_]]
    override def equals(other: Any) = other match {
      case other: BaseElem[_] => (this eq other) || (other.canEqual(this) && tag.tpe =:= other.tag.tpe)
      case _ => false
    }
    override def hashCode = tag.tpe.hashCode
  }

  case class PairElem[A, B](eFst: Elem[A], eSnd: Elem[B]) extends Element[(A, B)] {
    assert(eFst != null && eSnd != null)
    override def isEntityType = eFst.isEntityType || eSnd.isEntityType
    lazy val tag = {
      implicit val tA = eFst.tag
      implicit val tB = eSnd.tag
      weakTypeTag[(A, B)]
    }
    protected def getDefaultRep = Pair(eFst.defaultRepValue, eSnd.defaultRepValue)
  }

  case class SumElem[A, B](eLeft: Elem[A], eRight: Elem[B]) extends Element[A | B] {
    override def isEntityType = eLeft.isEntityType || eRight.isEntityType
    lazy val tag = {
      implicit val tA = eLeft.tag
      implicit val tB = eRight.tag
      weakTypeTag[A | B]
    }
    protected def getDefaultRep = mkLeft[A, B](eLeft.defaultRepValue)(eRight)
  }

  case class FuncElem[A, B](eDom: Elem[A], eRange: Elem[B]) extends Element[A => B] {
    override def isEntityType = eDom.isEntityType || eRange.isEntityType
    lazy val tag = {
      implicit val tA = eDom.tag
      implicit val tB = eRange.tag
      weakTypeTag[A => B]
    }
    protected def getDefaultRep = {
      val defaultB = eRange.defaultRepValue
      fun[A, B](_ => defaultB)(Lazy(eDom), eRange)
    }
  }

  val AnyRefElement: Elem[AnyRef] = new BaseElem[AnyRef]()(typeTag[AnyRef], Default.OfAnyRef)
  implicit val BooleanElement: Elem[Boolean] = new BaseElem[Boolean]
  implicit val ByteElement: Elem[Byte] = new BaseElem[Byte]
  implicit val ShortElement: Elem[Short] = new BaseElem[Short]
  implicit val IntElement: Elem[Int] = new BaseElem[Int]
  implicit val LongElement: Elem[Long] = new BaseElem[Long]
  implicit val FloatElement: Elem[Float] = new BaseElem[Float]
  implicit val DoubleElement: Elem[Double] = new BaseElem[Double]
  implicit val UnitElement: Elem[Unit] = new BaseElem[Unit]
  implicit val StringElement: Elem[String] = new BaseElem[String]
  implicit val CharElement: Elem[Char] = new BaseElem[Char]

  implicit def pairElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A, B)] =
    cachedElem[PairElem[A, B]](ea, eb)
  implicit def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A | B] =
    cachedElem[SumElem[A, B]](ea, eb)
  implicit def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B] =
    cachedElem[FuncElem[A, B]](ea, eb)
  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] =
    cachedElem[ScalaArrayElem[A]](eA)
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit def SumElemExtensions[A, B](eAB: Elem[A | B]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]
  implicit def ArrayElemExtensions[A](eArr: Elem[Array[A]]): ArrayElem[A] = eArr.asInstanceOf[ArrayElem[A]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

  object TagImplicits {
    implicit def elemToClassTag[A](implicit elem: Element[A]): ClassTag[A] = elem.classTag
    implicit def typeTagToClassTag[A](implicit tag: WeakTypeTag[A]): ClassTag[A] =
      ClassTag(tag.mirror.runtimeClass(tag.tpe))
  }

  def elemFromRep[A](x: Rep[A])(implicit eA: Elem[A]): Elem[A] = eA match {
    case ve: ViewElem[_,_] =>
      x.asRep[Def[_]].selfType1.asInstanceOf[Elem[A]]
    case pe: PairElem[a,b] =>
      implicit val ea = pe.eFst
      implicit val eb = pe.eSnd
      val pair = x.asRep[(a, b)]
      pairElement(elemFromRep(pair._1)(ea), elemFromRep(pair._2)(eb))
    case _ => eA
  }

  def assertEqualElems[A](e1: Elem[A], e2: Elem[A], m: => String) =
    assert(e1 == e2, s"Element $e1 != $e2: $m")

  import scalan.meta.ScalanAst
  import ScalanAst._
}

trait ElemsSeq extends Elems with Scalan { self: ScalanSeq =>

}

trait ElemsExp extends Elems
  with BaseExp
  with Scalan { self: ScalanExp =>

  def withElemOf[A, R](x: Rep[A])(block: Elem[A] => R) = block(x.elem)
  def withResultElem[A, B, R](f: Rep[A => B])(block: Elem[B] => R) = block(withElemOf(f) { e => e.eRange })

  //  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
  //    case ee: ElemElem[a] => ElemDef[a](x)
  //    case _ => super.toRep(x)(eA)
  //  }
}
